module Main where

import Control.Exception              
  ( catch
  , IOException )
import Control.Monad.Except
import qualified Control.Monad.Catch as MC
import Data.Char
import Data.List as L
import Data.Maybe
import System.Console.Haskeline
  ( defaultSettings
  , getInputLine
  , runInputT
  , InputT )
import System.Console.Readline ( readline )
import System.Environment
import System.IO hiding ( print )
import Data.Time ( Day, fromGregorian )
import Text.ParserCombinators.Parsec ( parse )

import Common
import PrettyPrinter
import Parse
import RecurrenceOps
import CalendarOps
import ICS

---------------------
--- Interpreter
---------------------

-- Función principal
main :: IO ()
main = runInputT defaultSettings main'

main' :: InputT IO ()
main' = do
  args <- lift getArgs
  readevalprint args (S True "" Null)

iname, iprompt :: String
iname = "calendario"
iprompt = "Cal> "

ioExceptionCatcher :: IOException -> IO (Maybe a)
ioExceptionCatcher _ = return Nothing

data State = S
  { inter :: Bool -- True, si estamos en modo interactivo.
  , file :: String -- Ultimo archivo cargado (para hacer "reload")
  , lfile :: Calendar -- Calendario cargado
  }

-- read-eval-print loop
readevalprint :: [String] -> State -> InputT IO ()
readevalprint args st =
  let 
    rec st = do
      mx <- MC.catch
        (if inter st then getInputLine iprompt else lift $ fmap Just getLine)
        (lift . ioExceptionCatcher)
      case mx of
        Nothing -> return ()
        Just "" -> rec st
        Just x  -> do
          c   <- interpretCommand x
          st' <- handleCommand st c
          maybe (return ()) rec st'
  in  
    do
      state' <- compileFiles args st
      when (inter st) $ lift $ putStrLn
        (  "Intérprete de "
        ++ iname
        ++ ".\n"
        ++ "Escriba :? para recibir ayuda."
        )
      --  enter loop
      rec state' { inter = True }

-- Parseo de comandos interactivos
commands :: [InteractiveCommand]
commands =
  [ Cmd [":load", ":l"] "<file>" Compile "Cargar un programa desde un archivo"
  , Cmd [":reload", ":r"] "" (const Reload) "Volver a cargar el último archivo"
  , Cmd [":print", ":p"] "" (const Print) "Imprime el calendario actual"
  , Cmd [":quit", ":q"] "" (const Quit) "Salir del intérprete"
  , Cmd [":help", ":?"] "" (const Help) "Mostrar esta lista de comandos"
  , Cmd [":ops", ":o"] "" (const Ops)  "Mostrar las operaciones del calendario"
  , Cmd [":close", ":c"] "" (const Close) "Cerrar y guardar el calendario actual"
  , Cmd [":export", ":e"] "" (const Export) "Exportar un calendario a un archivo .ical"
  , Cmd [":import", ":i"] "<file>" Import "Importar un calendario .ical"
  ]

-- Parseo de comandos de calendario
calCommands :: [CalendarCommand]
calCommands = 
  [ CCmd "newCalendar" ["<owner>"] "Crear un nuevo calendario"
  , CCmd "newEvent" ["<event>"] "Crear un nuevo evento"
  , CCmd "modify" ["<event>"] "Modificar un evento"
  , CCmd "delete" ["<event>"] "Eliminar un evento"
  , CCmd "search" ["<string>"] "Buscar un evento"
  , CCmd "day" ["<day>"] "Mostrar los eventos de un dia"
  , CCmd "week" ["<week>"] "Mostrar los eventos de una semana"
  , CCmd "month" ["<month>"] "Mostrar los eventos de un mes"
  , CCmd "allEvents" [] "Mostrar todos los eventos"
  , CCmd "category" ["<category>"] "Mostrar los eventos de una misma categoria"
  ]

interpretCommand :: String -> InputT IO Command
interpretCommand x = lift $ if ":" `L.isPrefixOf` x
  then do
    let (cmd, t') = L.break isSpace x
        t         = L.dropWhile isSpace t'
    -- find matching commands
        matching = L.filter (\(Cmd cs _ _ _) -> L.any (L.isPrefixOf cmd) cs) commands
    case matching of
      [] -> do
        putStrLn
          ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda."
          )
        return (ICom Noop)
      [Cmd _ _ f _] -> do
        return (ICom (f t))
      _ -> do
        putStrLn
          (  "Comando ambigüo, podría ser "
          ++ L.intercalate ", " ([ L.head cs | Cmd cs _ _ _ <- matching ])
          ++ "."
          )
        return (ICom Noop)
  else case parseThis x of
    Left _ -> do
      putStrLn
        "Operacion desconocida o argumentos invalidos. Escriba :o para recibir ayuda"
      return (ICom Noop)
    Right c -> return (CCom c)

handleCommand :: State -> Command -> InputT IO (Maybe State)
handleCommand state cmd = case cmd of
  (ICom c) -> handleInter state c
  (CCom c) -> handleCal state c

handleInter :: State -> InterCom -> InputT IO (Maybe State)
handleInter state@(S inter file lfile) cmd = case cmd of
  Quit -> lift $ unless inter (putStrLn "Quit") >> return Nothing
  Noop -> return (Just state)
  Help -> lift $ putStr (helpTxt commands) >> return (Just state)
  Print -> case lfile of
    Null -> lift $ putStrLn "No hay un archivo cargado.\n" >> return (Just state)
    c@(Calendar _ _) -> lift $ putStrLn (render (printCal c)) >> return (Just state)
  Compile f -> do
    state' <- compileFile (state { file = f }) f
    return (Just state')
  Reload -> case lfile of
    Null -> lift $ putStrLn "No hay un archivo cargado.\n" >> return (Just state)
    (Calendar _ _) -> handleCommand state (ICom (Compile file))
  Ops -> lift $ putStr (opsTxt calCommands) >> return (Just state)
  Close -> case lfile of
    Null -> lift $ putStrLn "No hay un archivo cargado.\n" >> return (Just state)
    (Calendar _ _) -> do 
      lift $ writeFile file (renderNoStyle $ printCal lfile)
      return (Just (state { lfile = Null }))
  Export -> case lfile of
    Null -> lift $ putStrLn "No hay un archivo cargado.\n" >> return (Just state)
    (Calendar _ _) -> do
      cal <- lift $ exportToIcs lfile
      return (Just state)
  Import f -> case lfile of
    Null -> lift $ putStrLn "No hay un archivo cargado.\n" >> return (Just state)
    (Calendar _ _) -> do
      cal <- lift $ importIcs f lfile
      return (Just state { file = f, lfile = cal })

handleCal :: State -> CalCom -> InputT IO (Maybe State)
handleCal state (NewCalendar n) = do 
  let new = newCalendar n
  return (Just (state { file = n ++ ".ical", lfile = new }))
handleCal state cmd = case lfile state of
  Null -> do lift $ putStrLn "Error: no hay un archivo cargado."
             return (Just state)
  Calendar n es -> handleCal' state (Calendar n es) cmd
  
handleCal' :: State -> Calendar -> CalCom -> InputT IO (Maybe State)
handleCal' state cal cmd = case cmd of
  NewEvent e -> case newEvent e (lfile state) of
    Left _ -> do lift $ putStrLn "Error: evento ya existe."
                 return (Just state)
    Right newCal -> return (Just (state { lfile = newCal }))
  ModifyEvent e -> do 
    x <- lift $ readline 
      ("Escriba la opción según la parte del evento " ++ show (summary e) ++ 
       " que quiera modificar:\n1. Título del evento.\n2. Horario/Día de incio.\n3. Horario/Día de finalización.\n4. Categoría.\n")
    case x of
      (Just "1") -> do
        y <- lift $ readline "Escriba el nuevo título.\n"
        let Calendar n ev = lfile state
            ev' = modifyTitle e ev (fromJust y)
        return (Just (state { lfile = Calendar n ev' }))
      (Just "2") -> do
        y <- lift $ readline "Escriba el nuevo horario/día de inicio (d/m/a h:min).\n"
        case parse parseDate "" (fromJust y) of
          Left e -> lift $ print e >> return Nothing
          Right (st, _) -> do
            let Calendar n ev = lfile state
                ev' = modifyST e ev st
            return (Just (state { lfile = Calendar n ev' }))
      (Just "3") -> do
        y <- lift $ readline "Escriba el nuevo horario/día de finalización (d/m/a h:min).\n"
        case parse parseDate "" (fromJust y) of
          Left e -> lift $ print e >> return Nothing
          Right (et, _) -> do
            let Calendar n ev = lfile state
                ev' = modifyET e ev et
            return (Just (state { lfile = Calendar n ev' }))
      (Just "4") -> do
        y <- lift $ readline "Escriba la nueva categoría.\n"
        let Calendar n ev = lfile state
            ev' = modifyCategory e ev (fromJust y)
        return (Just (state { lfile = Calendar n ev' }))
      (Just _) -> do lift $ putStrLn "Error: se espera un número del 1 al 5." >> return (Just state) 
  DeleteEvent e -> case deleteEvent e cal of
    Left _ -> do lift $ putStrLn ("Error: evento" ++ summary e ++ "no existe.")
                 return (Just state)
    Right newCal -> return (Just (state { lfile = newCal }))
  SearchEvent s -> do
    let Calendar n ev = lfile state
    lift $ putStrLn ("Eventos " ++ s ++ " encontrados:\n\n" ++
                    ppListEv (searchEvent s ev))
    return (Just state)
  ThisDay -> do 
    liftIO $ timeline (thisDay cal)
    return (Just state) 
  ThisWeek -> do 
    liftIO $ weekly (thisWeek cal)
    return (Just state)
  ThisMonth -> do 
    liftIO $ monthly (thisMonth cal)
    return (Just state)
  AllEvents -> do 
    liftIO $ table (allEvents cal)
    return (Just state)
  Category c -> do
    let Calendar n ev = lfile state
    lift $ putStrLn ("Eventos categorizados como " ++ c ++ ":\n\n" ++
                    ppListEv (sameCategory ev c))
    return (Just state)

helpTxt :: [InteractiveCommand] -> String
helpTxt cs =
  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n"
    ++ "c es el primer caracter del nombre completo.\n\n"
    ++ L.unlines 
      (L.map
        (\(Cmd c a _ d) -> 
          let ct = L.intercalate ", "
                   (L.map (++ if L.null a then "" else " " ++ a) c)
          in ct ++ L.replicate ((24 - L.length ct) `max` 2) ' ' ++ d
        )
        cs
      )                

opsTxt :: [CalendarCommand] -> String
opsTxt cs =
  "Lista de operaciones del calndario:\n\n"
  ++ L.unlines
      (L.map 
        (\(CCmd a c d) -> 
          let ct = L.intercalate ", "
                   (L.map (++ if L.null a then "" else " " ++ a) c)
          in ct ++ L.replicate ((24 - L.length ct) `max` 2) ' ' ++ d
        )
        cs
      )

compileFiles :: [String] -> State -> InputT IO State
compileFiles xs s =
  foldM (\s x -> compileFile (s { file = x, inter = False }) x) s xs

compileFile :: State -> String -> InputT IO State
compileFile state@(S inter file lfile) f =
  if f == ".cal" then return state
  else do
    lift $ putStrLn ("Abriendo " ++ f ++ "...")
    let f' = L.reverse (L.dropWhile isSpace (L.reverse f))
    x <- lift $ Control.Exception.catch
      (readFile f')
      (\e -> do
        let err = show (e :: IOException)
        hPutStr stderr
          ("No se pudo abrir el archivo " ++ f' ++ ": " ++ err ++ "\n")
        return ""
      )
    cal <- case parse (totParser parseCal) f x of
      Left e -> lift $ print e >> return Nothing
      Right c -> return (Just c)
    maybe (return state) (\s -> return (state { file = f, lfile = s })) cal
