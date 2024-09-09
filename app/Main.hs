module Main where

import           Control.Exception              ( catch
                                                , IOException
                                                )
import           Control.Monad.Except
import qualified Control.Monad.Catch            as MC
import           Data.Char
import           Data.List
import           Data.Maybe
-- import           Data.Text.Lazy.IO as TIO
import           Prelude                 hiding ( print )
import           System.Console.Haskeline
import           System.Environment
import           System.IO               hiding ( print )
import           Text.PrettyPrint.HughesPJ      ( render
                                                , text
                                                )
import           Data.Text (pack)
import           Data.Time (Day, fromGregorian)
import           Text.ParserCombinators.Parsec (parse)

import           Common
import           PrettyPrinter
import           Parse
import           CalendarOps
import           ICal
---------------------
--- Interpreter
---------------------

-- Función principal
main :: IO ()
main = runInputT defaultSettings main'

main' :: InputT IO ()
main' = do
  args <- lift getArgs
  return ()
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
  let rec st = do
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
  in  do
        state' <- compileFiles (prelude : args) st
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
  [ Cmd [":load", ":l"] "<file>" (Compile) "Cargar un programa desde un archivo"
  , Cmd [":reload", ":r"] "<file>" (const Recompile) "Volver a cargar el último archivo"
  -- , Cmd [":print", ":p"] "<exp>" (const Print) "Imprime el calendario actual"
  , Cmd [":quit", ":q"] "" (const Quit) "Salir del intérprete"
  , Cmd [":help", ":?"] "" (const Help) "Mostrar esta lista de comandos"
  , Cmd [":ops", ":o"] "" (const Ops)  "Mostrar las operaciones del calendario"
  -- , Cmd [":save", ":s"] "" (const Save) "Guardar el calendario actual"
  , Cmd [":close", ":c"] "" (const Close) "Cerrar y guardar el calendario actual"
  , Cmd [":export", ":e"] "<file>" (Export) "Exportar un calendario a un archivo .ical"
  -- , Cmd [":import", ":i"] "<file>" (Import) "Importar un calendario .ical"
  ]

-- Parseo de comandos de calendario
calCommands :: [CalendarCommand]
calCommands = 
  [ CCmd "newCalendar" ["<owner>"] "Crear un nuevo calendario"
  , CCmd "newEvent" ["<summary>", "<startDate>", "<endDate>", "<category>"] "Crear un nuevo evento"
  , CCmd "addEvent" ["<event>"] "Agregar un evento"
  , CCmd "deleteEvent" ["<event>"] "Eliminar un evento"
  , CCmd "day" ["<day>"] "Mostrar los eventos de un dia"
  , CCmd "week" ["<week>"] "Mostrar los eventos de una semana"
  , CCmd "month" ["<month>"] "Mostrar los eventos de un mes"
  , CCmd "allEvents" [] "Mostrar todos los eventos"
  ]

interpretCommand :: String -> InputT IO Command
interpretCommand x = lift $ if isPrefixOf ":" x
  then do
    let (cmd, t') = break isSpace x  -- ejemplo: "cmd arg1 arg2" -> ("cmd", " arg1 arg2")
        t         = dropWhile isSpace t'  -- ejemplo: " arg1 arg2" -> "arg1 arg2"
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
          ++ concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ])
          ++ "."
          )
        return (ICom Noop)
  else case parseCal x of
    Left _ -> do
      putStrLn
        ("Operacion desconocida o argumentos invalidos. Escriba :o para recibir ayuda"
        )
      return (ICom Noop)
    Right c -> return (CCom c)

handleCommand :: State -> Command -> InputT IO (Maybe State)
handleCommand state cmd = case cmd of
  (ICom c) -> handleInter state c
  (CCom c) -> handleCal state c

handleInter :: State -> InterCom -> InputT IO (Maybe State)
handleInter state@(S inter file lfile) cmd = case cmd of
  Quit   -> lift $ when (not inter) (putStrLn "Quit") >> return Nothing
  Noop   -> return (Just state)
  Help   -> lift $ putStr (helpTxt commands) >> return (Just state)
  Compile f -> do
    state' <- compileFile (state { file = f }) f
    return (Just state')
  Recompile -> case lfile of
    Null -> lift $ putStrLn "No hay un archivo cargado.\n" >> return (Just state)
    (Calendar _ _) -> handleCommand state (ICom (Compile file))
  Ops ->
    lift $ putStr (opsTxt calCommands) >> return (Just state)
  Close -> case lfile of
    Null -> lift $ putStrLn "No hay un archivo cargado.\n" >> return (Just state)
    (Calendar _ _) -> do 
      lift $ writeFile file (render $ printCal lfile)
      return (Just (state { lfile = Null }))
  Export f -> do
    cal <- lift $ exportToICal f lfile
    return (Just state)
  -- Import f -> do
  --   cal <- lift $ importICal f
  --   return (Just state { file = f, lfile = updateCalendar state cal })

handleCal :: State -> CalCom -> InputT IO (Maybe State)
handleCal state (NewCalendar n) = do 
  let new = newCalendar n
  return (Just (state { file = (n ++ ".ical"), lfile = new }))
handleCal state cmd = case lfile state of
  Null -> do lift $ putStrLn "Error: no hay un archivo cargado."
             return (Just state)
  Calendar n es -> handleCal' state (Calendar n es) cmd
  
handleCal' :: State -> Calendar -> CalCom -> InputT IO (Maybe State)
handleCal' state cal cmd = case cmd of
  NewEvent s st et c -> undefined
  AddEvent e -> case (addEvent e (lfile state)) of
    Left _ -> do lift $ putStrLn "Error: evento ya existe."
                 return (Just state)
    Right newCal -> return (Just (state { lfile = newCal }))
  DeleteEvent e -> case (deleteEvent e cal) of
    Left _ -> do lift $ putStrLn ("Error: evento" ++ show e ++ "no existe.")
                 return (Just state)
    Right newCal -> return (Just (state { lfile = newCal }))
  ThisDay -> do lift $ putStrLn ("Eventos de hoy:\n\n" ++ 
                                printListEvent (thisDay cal))
                return (Just state) 
  ThisWeek -> do lift $ putStrLn ("Eventos de esta semana:\n\n" ++
                                 printListEvent (thisWeek cal))
                 return (Just state)
  ThisMonth -> do lift $ putStrLn ("Eventos de este mes:\n\n" ++
                                  printListEvent (thisMonth cal))
                  return (Just state)
  AllEvents -> do lift $ putStrLn ("Todos los eventos:\n\n" ++
                                  printListEvent (allEvents cal))
                  return (Just state)

helpTxt :: [InteractiveCommand] -> String
helpTxt cs =
  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n"
    ++ "c es el primer caracter del nombre completo.\n\n"
    ++ unlines 
      (map
        (\(Cmd c a _ d) -> 
          let 
            ct = 
              concat
                (intersperse ", "
                  (map (++ if null a then "" else " " ++ a) c)
                )
          in ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d
        )
        cs
      )                

opsTxt :: [CalendarCommand] -> String
opsTxt cs =
  "Lista de operaciones del calndario:\n\n"
  ++ unlines
      (map 
        (\(CCmd a c d) -> 
          let 
            ct =
              concat
                (intersperse ", "
                  (map (++ if null a then "" else " " ++ a) c)
                )
          in ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d
        )
        cs
      )

compileFiles :: [String] -> State -> InputT IO State
compileFiles xs s =
  foldM (\s x -> compileFile (s { file = x, inter = False }) x) s xs

compileFile :: State -> String -> InputT IO State
compileFile state@(S inter file lfile) f = do
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
    Left e -> lift $ putStrLn (show e) >> return Nothing
    Right c -> return (Just state { lfile = c })
  maybe (return state) (\s -> return (state { file = f, lfile = s })) cal

-- compilePhrase :: State -> String -> InputT IO State
-- compilePhrase state x = do
--   x' <- parseIO "<interactive>" stmt_parse x
--   maybe (return state) (handleStmt state) x'

-- printPhrase :: String -> InputT IO ()
-- printPhrase x = do
--   x' <- parseIO "<interactive>" stmt_parse x
--   maybe (return ()) (printStmt . fmap (\y -> (y, conversion y))) x'

-- printStmt :: Stmt (LamTerm, Term) -> InputT IO ()
-- printStmt stmt = lift $ do
--   let outtext = case stmt of
--         Def x (_, e) -> "def " ++ x ++ " = " ++ render (printTerm e)
--         Eval (d, e) ->
--           "LamTerm AST:\n"
--             ++ show d
--             ++ "\n\nTerm AST:\n"
--             ++ show e
--             ++ "\n\nSe muestra como:\n"
--             ++ render (printTerm e)
--   putStrLn outtext

-- parseIO :: String -> (String -> ParseResult a) -> String -> InputT IO (Maybe a)
-- parseIO f p x = lift $ case p x of
--   Failed e -> do
--     putStrLn (f ++ ": " ++ e)
--     return Nothing
--   Ok r -> return (Just r)

-- handleStmt :: State -> Stmt LamTerm -> InputT IO State
-- handleStmt state stmt = lift $ do
--   case stmt of
--     Def x e -> checkType x (conversion e)
--     Eval e  -> checkType it (conversion e)
--  where
--   checkType i t = do
--     case infer (ve state) t of
--       Left  err -> putStrLn ("Error de tipos: " ++ err) >> return state
--       Right ty  -> checkEval i t ty
--   checkEval i t ty = do
--     let v = eval (ve state) t
--     _ <- when (inter state) $ do
--       let outtext =
--             if i == it then render (printTerm (quote v)) else render (text i)
--       putStrLn outtext
--     return (state { ve = (Global i, (v, ty)) : ve state })
 
prelude :: String
prelude = "Ejemplos/Prelude.lam"

-- it :: String
-- it = "it"
