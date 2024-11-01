module Main where

import Control.Exception              
  ( catch
  , IOException )
import Control.Monad.Except hiding ( liftEither )
import qualified Control.Monad.Catch as MC
import Data.Char
import Data.List
import Data.Maybe
import System.Console.Haskeline
  ( defaultSettings
  , getInputLine
  , runInputT
  , InputT )
import System.Console.Readline ( readline )
import System.Environment
import System.IO hiding ( print )
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
  readevalprint args (S "" Null)

iname, iprompt :: String
iname = "calendario"
iprompt = "Cal> "

ioExceptionCatcher :: IOException -> IO (Maybe a)
ioExceptionCatcher _ = return Nothing

data State = S
  { file :: String -- Ultimo archivo cargado (para hacer "reload")
  , lfile :: Calendar -- Calendario cargado
  }

-- read-eval-print loop
readevalprint :: [String] -> State -> InputT IO ()
readevalprint args st =
  let 
    rec st = do
      mx <- MC.catch
        (getInputLine iprompt)
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
      lift $ putStrLn
        (  "Intérprete de "
        ++ iname
        ++ ".\n"
        ++ "Escriba :? para recibir ayuda."
        )
      --  enter loop
      rec st

-- Parseo de comandos interactivos
commands :: [InteractiveCommand]
commands =
  [ Cmd [":load", ":l"] "<file>" Compile "Cargar un programa desde un archivo"
  , Cmd [":reload", ":r"] "" (const Reload) "Volver a cargar el último archivo"
  , Cmd [":print", ":p"] "" (const Print) "Imprime el calendario actual"
  , Cmd [":quit", ":q"] "" (const Quit) "Salir del intérprete"
  , Cmd [":help", ":?"] "" (const Help) "Mostrar esta lista de comandos"
  , Cmd [":ops", ":o"] "" (const Ops) "Mostrar las operaciones del calendario"
  , Cmd [":close", ":c"] "" (const Close) "Cerrar y guardar el calendario actual"
  , Cmd [":export", ":e"] "" (const Export) "Exportar un calendario a un archivo .ics"
  , Cmd [":import", ":i"] "<file>" Import "Importar un calendario .ics"
  ]

-- Parseo de comandos de calendario
calCommands :: [CalendarCommand]
calCommands = 
  [ CCmd "C" ["<owner>"] "Crear un nuevo calendario"
  , CCmd "E" ["<summary>", "<start>", "-", "<end>"] "Crear un nuevo evento"
  , CCmd "modify" ["<event>"] "Modificar un evento"
  , CCmd "delete" ["<event>"] "Eliminar un evento"
  , CCmd "search" ["<string>"] "Buscar un evento"
  , CCmd "day" [] "Mostrar los eventos de hoy"
  , CCmd "week" [] "Mostrar los eventos de esta semana"
  , CCmd "month" [] "Mostrar los eventos de este mes"
  , CCmd "all" [] "Mostrar todos los eventos"
  , CCmd "category" ["<category>"] "Mostrar los eventos de una misma categoria"
  ]

interpretCommand :: String -> InputT IO Command
interpretCommand x = lift $ if ":" `isPrefixOf` x
  then do
    let (cmd, t') = break isSpace x
        t         = dropWhile isSpace t'
    -- find matching commands
        matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
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
          ++ intercalate ", " ([ head cs | Cmd cs _ _ _ <- matching ])
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
handleInter _ Quit = return Nothing
handleInter st Noop = return (Just st)
handleInter st Help = lift $ putStr (helpTxt commands) >> return (Just st)
handleInter st Print = 
  case lfile st of
    Null -> lift $ putStrLn "No hay un archivo cargado." >> return (Just st)
    c -> lift $ putStrLn (render (printCal c)) >> return (Just st)
handleInter st (Compile f) = do
  st' <- compileFile (st { file = f }) f
  return (Just st')
handleInter st Reload = 
  case lfile st of
    Null -> lift $ putStrLn "No hay un archivo cargado." >> return (Just st)
    c -> handleCommand st (ICom (Compile $ file st))
handleInter st Ops = lift $ putStr (opsTxt calCommands) >> return (Just st)
handleInter st Close = 
  case lfile st of
    Null -> lift $ putStrLn "No hay un archivo cargado." >> return (Just st)
    c -> do 
      lift $ writeFile (file st) (renderNoStyle $ printCal $ lfile st)
      return (Just (st { lfile = Null }))
handleInter st Export = 
  case lfile st of
    Null -> lift $ putStrLn "No hay un archivo cargado." >> return (Just st)
    c -> do
      cal <- lift $ exportToIcs $ lfile st
      return (Just st)
handleInter st (Import f) = 
  case lfile st of
    Null -> lift $ putStrLn "No hay un archivo cargado." >> return (Just st)
    c -> do
      cal <- lift $ importIcs f (lfile st)
      return (Just st { lfile = cal })

handleCal :: State -> CalCom -> InputT IO (Maybe State)
handleCal st (NewCalendar n) = do 
  let new = newCalendar n
  return (Just (st { file = n ++ ".cal", lfile = new }))
handleCal st cmd = 
  case lfile st of
    Null -> do 
      lift $ putStrLn "Error: no hay un archivo cargado."
      return (Just st)
    c -> handleCal' st c cmd
  
handleCal' :: State -> Calendar -> CalCom -> InputT IO (Maybe State)
handleCal' st cal (NewEvent e) = 
  case newEvent e cal of
    Left _ -> do 
      lift $ putStrLn "Error: el evento ya existe."
      return (Just st)
    Right newCal -> return (Just (st { lfile = newCal }))
handleCal' state c@(Calendar n ev) (ModifyEvent e) = do 
  x <- lift $ readline 
    ("Escriba la opción según la parte del evento " ++ show (summary e) ++ 
     " que quiera modificar:\n1. Título del evento.\n2. Horario/Día de incio.\n3. Horario/Día de finalización.\n4. Categoría.\n")
  case x of
    (Just "1") -> do
      y <- lift $ readline "Escriba el nuevo título.\n"
      let ev' = modifyTitle e ev (fromJust y)
      return (Just (state { lfile = Calendar n ev' }))
    (Just "2") -> do
      y <- lift $ readline "Escriba el nuevo horario/día de inicio (d/m/a h:min).\n"
      case parse parseDate "" (fromJust y) of
        Left e -> lift $ print e >> return (Just state)
        Right (st, _) -> do
          let ev' = modifyST e ev st
          return (Just (state { lfile = Calendar n ev' }))
    (Just "3") -> do
      y <- lift $ readline "Escriba el nuevo horario/día de finalización (d/m/a h:min).\n"
      case parse parseDate "" (fromJust y) of
        Left e -> lift $ print e >> return (Just state)
        Right (et, _) -> do
          let ev' = modifyET e ev et
          return (Just (state { lfile = Calendar n ev' }))
    (Just "4") -> do
      y <- lift $ readline "Escriba la nueva categoría.\n"
      let ev' = modifyCategory e ev (fromJust y)
      return (Just (state { lfile = Calendar n ev' }))
    (Just _) -> do lift $ putStrLn "Error: se espera un número del 1 al 4." >> return (Just state) 
handleCal' st cal (DeleteEvent e) =
  case deleteEvent e cal of
    Left _ -> do 
      lift $ putStrLn ("Error: evento" ++ summary e ++ "no existe.")
      return (Just st)
    Right newCal -> return (Just (st { lfile = newCal }))
handleCal' st (Calendar n ev) (SearchEvent s) = do
  lift $ table (searchEvent s ev) ("Eventos " ++ s ++ " encontrados:")
  return (Just st)
handleCal' st cal ThisDay = do 
  lift $ timeline (thisDay cal)
  return (Just st)
handleCal' st cal ThisWeek =do 
  lift $ weekly (thisWeek cal)
  return (Just st)
handleCal' st cal ThisMonth = do 
  lift $ monthly (thisMonth cal)
  return (Just st)
handleCal' st cal AllEvents = do 
  lift $ table (allEvents cal) "Todos los eventos:"
  return (Just st)
handleCal' st (Calendar n ev) (Category c) = do
  lift $ table (sameCategory ev c) ("Eventos categorizados como " ++ c ++ ":")
  return (Just st)

helpTxt :: [InteractiveCommand] -> String
helpTxt cs =
  "Lista de comandos:\n\n"
    ++ unlines 
      (map
        (\(Cmd c a _ d) -> 
          let ct = intercalate ", "
                   (map (++ if null a then "" else " " ++ a) c)
          in ct ++ replicate ((30 - length ct) `max` 2) ' ' ++ d
        )
        cs
      )                

opsTxt :: [CalendarCommand] -> String
opsTxt cs =
  "Lista de operaciones del calndario:\n\n"
  ++ unlines 
    (map 
      (\(CCmd c a d) -> 
        let ct = c ++ " " ++ unwords a
        in ct ++ replicate ((30 - length ct) `max` 2) ' ' ++ d
      )
      cs
    )

compileFiles :: [String] -> State -> InputT IO State
compileFiles xs s =
  foldM (\s x -> compileFile (s { file = x }) x) s xs

compileFile :: State -> String -> InputT IO State
compileFile st f =
  if f == ".cal" then return st
  else do
    lift $ putStrLn ("Abriendo " ++ f ++ "...")
    let f' = reverse (dropWhile isSpace (reverse f))
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
      Right (Calendar n es) -> do
        res <- foldM (\cal ev -> liftEither (newEvent ev cal)) (Calendar n []) es
        return (Just res)
    maybe (return st) (\s -> return (st { file = f, lfile = s })) cal

liftEither :: Either Error Calendar -> InputT IO Calendar
liftEither (Left _) = lift $ putStrLn "Error en newEvent." >> return Null
liftEither (Right cal) = return cal
