module Main where

import           Control.Exception              ( catch
                                                , IOException
                                                )
import           Control.Monad.Except
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Text.Lazy.IO as TIO
import           Prelude                 hiding ( print )
import           System.Console.Haskeline
import qualified Control.Monad.Catch           as MC
import           System.Environment
import           System.IO               hiding ( print )
import           Text.PrettyPrint.HughesPJ      ( render
                                                , text
                                                )
import           Text.ICalendar (VCalendar, VEvent(..), DTStamp(..), dtStamp, event)
import           Text.ICalendar.Printer (printICalendar)
import           Data.Time (Day, fromGregorian)
import           Data.VCard

import           Common
import           PrettyPrinter
import           Simplytyped
import           Parse
---------------------
--- Interpreter
---------------------

-- Función para crear un evento de calendario
crearEvento :: Day -> String -> String -> VEvent
crearEvento f r d =
  mkVEvent 1 f (Just r) Nothing Nothing Nothing Nothing Nothing Nothing (Just d) Nothing Nothing Nothing Nothing

-- Función principal
main :: IO ()
main = runInputT defaultSettings main'

main' :: InputT IO ()
main' = do
  args <- lift getArgs
  readevalprint args (S True "" [])

iname, iprompt :: String
iname = "calendario"
iprompt = "Cal> "

ioExceptionCatcher :: IOException -> IO (Maybe a)
ioExceptionCatcher _ = return Nothing

data State = S
  { inter :: Bool -- True, si estamos en modo interactivo.
  , lfile :: String -- Ultimo archivo cargado (para hacer "reload")
  }

--  read-eval-print loop
readevalprint :: [String] -> State -> InputT IO ()
readevalprint args state@(S inter lfile ve) =
  let rec st = do
        mx <- MC.catch
          (if inter then getInputLine iprompt else lift $ fmap Just getLine)
          (lift . ioExceptionCatcher)
        case mx of
          Nothing -> return ()
          Just "" -> rec st
          Just x  -> do
            c   <- interpretCommand x
            st' <- handleCommand st c
            maybe (return ()) rec st'
  in  do
        state' <- compileFiles (prelude : args) state
        when inter $ lift $ putStrLn
          (  "Intérprete de "
          ++ iname
          ++ ".\n"
          ++ "Escriba :? para recibir ayuda."
          )
        --  enter loop
        rec state' { inter = Tru }

data CalCom = NewCalendar Name
            | NewEvent DateTime String Category
            | AddEvent Event
            | DeleteEvent Event
            | ThisDay
            | ThisWeek
            | ThisMonth
            | AllEvents deriving Show

data InterCom = Compile ComepileForm
              | Recompile
              | Print
              | Quit
              | Help
              | Noop
              | Ops
              | Save
              | Close 
              | Export String deriving Show

data CompileForm = CompileInteractive  String
                 | CompileFile         String

data Command = CCom CalCom | ICom InterCom

data InteractiveCommand = Cmd [String] String (String -> InterCom) String

data CalendarCommand = CCmd String [String] String

interpretCommand :: String -> InputT IO Command
interpretCommand x = lift $ if isPrefixOf ":" x
  then do
    let (cmd, t') = break isSpace x
        t         = dropWhile isSpace t'
    --  find matching commands
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
        ("Operacion desconocida o argumentos invalidos. Escriba :o para recibir ayuda")
        return (ICom Noop)
    Right c -> return (CCom c)

handleCommand :: State -> Command -> InputT IO (Maybe State)
handleCommand state cmd = case cmd of
  (ICom c) -> handleInter state c
  (CCom c) -> handleCal state c

handleInter :: State -> InterCom -> InputT IO (Maybe State)
handleInter state@(S inter lfile ve) cmd = case cmd of
  Quit   -> lift $ when (not inter) (putStrLn "Quit") >> return Nothing
  Noop   -> return (Just state)
  Help   -> lift $ putStr (helpTxt commands) >> return (Just state)
  Compile c -> do
    state' <- case c of
      CompileInteractive s -> compilePhrase state s
      CompileFile        f -> compileFile (state { lfile = f }) f
    return (Just state')
  Print s ->
    lift $ putStr (render (printCal ...)) >> return (Just state)
  Recompile -> if null lfile
    then lift $ putStrLn "No hay un archivo cargado.\n" >> return (Just state)
    else handleCommand state (Compile (CompileFile lfile))
  Ops ->
    lift $ putStr (opsTxt calCommands) >> return (Just state)
  Save -> undefined
  Close -> undefined
  Export f -> do
    cal <- lift $ exportToICal f ve
    return (Just state)

handleCal :: State -> Calendar -> CalCom -> InputT IO (Maybe State)
handleCal state cal cmd = case cmd of
  AddEvent e -> 

-- Parseo de comandos interactivos
commands :: [InteractiveCommand]
commands =
  [ Cmd [":load", ":l"] "<file>" (Compile . CompileFile) "Cargar un programa desde un archivo"
  , Cmd [":reload", ":r"] "<file>" (const Recompile) "Volver a cargar el último archivo"
  , Cmd [":print", ":p"] "" (const Print) "Imprime el calendario actual"
  , Cmd [":quit", ":q"] "" (const Quit) "Salir del intérprete"
  , Cmd [":help", ":?"] "" (const Help) "Mostrar esta lista de comandos"
  , Cmd [":ops", ":o"] "" (const Ops)  "Mostrar las operaciones del calendario"
  , Cmd [":save", ":s"] "" (const Save) "Guardar el calendario actual"
  , Cmd [":close", ":c"] "" (const Close) "Cerrar y guardar el calendario actual"
  , Cmd [":export", ":e"] "<file>" (Export) "Exportar un calendario a un archivo .ical"
  ]

-- Parseo de comandos de calendario
calCommands :: [CalendarCommand]
calCommands = 
  [ CCmd "newCalendar" ["<owner>"] "Crear un nuevo calendario"
  , CCmd "newEvent" ["<date>", "<description>", "<category>"] "Crear un nuevo evento"
  , CCmd "addEvent" ["<event>"] "Agregar un evento"
  , CCmd "deleteEvent" ["<event>"] "Eliminar un evento"
  , CCmd "day" ["<day>"] "Mostrar los eventos de un dia"
  , CCmd "week" ["<week>"] "Mostrar los eventos de una semana"
  , CCmd "month" ["<month>"] "Mostrar los eventos de un mes"
  , CCmd "allEvents" [] "Mostrar todos los eventos"
  ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs =
  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n"
    ++ "c es el primer caracter del nombre completo.\n\n"
    ++ "<expr>                  evaluar la expresión\n"
    ++ "def <var> = <expr>      definir una variable\n"
    ++ unlines
         (map
           (\(Cmd c a _ d) ->
             let
               ct =
                 concat
                   (intersperse ", "
                                (map (++ if null a then "" else " " ++ a) c)
                   )
             in  ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d
           )
           cs
         )

exportToICal :: FilePath -> [Event] -> IO ()
exportToICal file events = do contents <- readFile file
                     

compileFiles :: [String] -> State -> InputT IO State
compileFiles xs s =
  foldM (\s x -> compileFile (s { lfile = x, inter = False }) x) s xs

compileFile :: State -> String -> InputT IO State
compileFile state@(S inter lfile v) f = do
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
  stmts <- parseIO f' (stmts_parse) x
  maybe (return state) (foldM handleStmt state) stmts


compilePhrase :: State -> String -> InputT IO State
compilePhrase state x = do
  x' <- parseIO "<interactive>" stmt_parse x
  maybe (return state) (handleStmt state) x'

printPhrase :: String -> InputT IO ()
printPhrase x = do
  x' <- parseIO "<interactive>" stmt_parse x
  maybe (return ()) (printStmt . fmap (\y -> (y, conversion y))) x'

printStmt :: Stmt (LamTerm, Term) -> InputT IO ()
printStmt stmt = lift $ do
  let outtext = case stmt of
        Def x (_, e) -> "def " ++ x ++ " = " ++ render (printTerm e)
        Eval (d, e) ->
          "LamTerm AST:\n"
            ++ show d
            ++ "\n\nTerm AST:\n"
            ++ show e
            ++ "\n\nSe muestra como:\n"
            ++ render (printTerm e)
  putStrLn outtext

parseIO :: String -> (String -> ParseResult a) -> String -> InputT IO (Maybe a)
parseIO f p x = lift $ case p x of
  Failed e -> do
    putStrLn (f ++ ": " ++ e)
    return Nothing
  Ok r -> return (Just r)

handleStmt :: State -> Stmt LamTerm -> InputT IO State
handleStmt state stmt = lift $ do
  case stmt of
    Def x e -> checkType x (conversion e)
    Eval e  -> checkType it (conversion e)
 where
  checkType i t = do
    case infer (ve state) t of
      Left  err -> putStrLn ("Error de tipos: " ++ err) >> return state
      Right ty  -> checkEval i t ty
  checkEval i t ty = do
    let v = eval (ve state) t
    _ <- when (inter state) $ do
      let outtext =
            if i == it then render (printTerm (quote v)) else render (text i)
      putStrLn outtext
    return (state { ve = (Global i, (v, ty)) : ve state })

prelude :: String
prelude = "Ejemplos/Prelude.lam"

it :: String
it = "it"


