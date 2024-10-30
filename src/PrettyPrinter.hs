module PrettyPrinter where

import System.IO.Unsafe ( unsafePerformIO )
import Data.Text ( unpack )
import Data.List ( sortBy )
import Data.Ord ( comparing )
import Data.Time.Calendar.WeekDate ( toWeekDate )
import Prettyprinter.Render.Terminal
  ( renderStrict, italicized, bold
  , underlined, color, colorDull
  , Color (..), AnsiStyle, putDoc )
import Prettyprinter.Render.String (renderString)
import Prettyprinter
  ( (<+>), annotate, defaultLayoutOptions,
    layoutSmart, nest, line, comma, colon,
    sep, parens, brackets, space, unAnnotate,
    Doc, Pretty(pretty), vsep, indent, hsep, 
    align, vcat, hardline, hcat )

import Common
import Data.Time (fromGregorian, toGregorian, utctDay, getCurrentTime, Day, gregorianMonthLength)

-- | Colores
-- |
constColor :: Doc AnsiStyle -> Doc AnsiStyle
constColor = annotate (bold <> colorDull Green)

sepColor :: Doc AnsiStyle -> Doc AnsiStyle
sepColor = annotate bold

keywordColor :: Doc AnsiStyle -> Doc AnsiStyle
keywordColor = annotate (italicized <> color Blue)

titleColor :: Doc AnsiStyle -> Doc AnsiStyle
titleColor = annotate (italicized <> bold <> underlined <> color Green)

nameColor :: Doc AnsiStyle -> Doc AnsiStyle
nameColor = annotate (bold <> color Green)

-- | Documentos
-- |
constDoc :: String -> Doc AnsiStyle
constDoc s = constColor (pretty s)

intDoc :: Int -> Doc AnsiStyle
intDoc n = sepColor (pretty (show n))

sepDoc :: String -> Doc AnsiStyle
sepDoc s = sepColor (pretty s)

keywordDoc :: String -> Doc AnsiStyle
keywordDoc s = keywordColor (pretty s)

titleDoc :: String -> Doc AnsiStyle
titleDoc s = titleColor (pretty s)

nameDoc :: String -> Doc AnsiStyle
nameDoc s = nameColor (pretty s)

-- | Imprime una fecha
-- |
first :: DateTime -> Int
first (DateTime (d, _, _, _, _)) = d

second :: DateTime -> Int
second (DateTime (_, m, _, _, _)) = m

third :: DateTime -> Int
third (DateTime (_, _, y, _, _)) = y

fourth :: DateTime -> Int
fourth (DateTime (_, _, _, h, _)) = h

fifth :: DateTime -> Int
fifth (DateTime (_, _, _, _, m)) = m

printDate :: DateTime -> Doc AnsiStyle
printDate date = 
  let d = first date
      m = second date
      y = third date
      h = fourth date
      mi = fifth date
  in intDoc d <> sepDoc "/" <> intDoc m <> sepDoc "/"
     <> intDoc y <+> intDoc h <> sepDoc ":" <> intDoc mi

-- | Imprime un evento
-- |
printEvent :: Event -> Doc AnsiStyle
printEvent (Event s st et Nothing r b) = 
  keywordDoc "E" <+> nameDoc s <+> printDate st <+> sepDoc ">" <+> printDate et
printEvent (Event s st et (Just c) r b) =
  keywordDoc "E" <+> nameDoc s <+> printDate st <+> sepDoc ">" <+> printDate et <+> nameDoc c

-- | Imprime una lista
-- |
printList :: [a] -> (a -> Doc AnsiStyle) -> Doc AnsiStyle
printList [] _ = mempty
printList [x] p = p x <> line
printList (x:xs) p = 
  p x <> comma <> line <> printList xs p

-- | Imprime una lista de eventos
-- |
printListEvent :: [Event] -> Doc AnsiStyle
printListEvent es = printList es printEvent

-- | Imprime un calendario
-- |
printCal :: Calendar -> Doc AnsiStyle
printCal (Calendar u es) = 
  keywordDoc "C" <+> nameDoc u <> line <> printListEvent es

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

renderNoStyle :: Doc AnsiStyle -> String
renderNoStyle = renderString . layoutSmart defaultLayoutOptions . unAnnotate

ppListEv :: [Event] -> String
ppListEv = render . printListEvent

-------------------------------------------------------------
-------------------------------------------------------------
{- 
  Operaciones para las funciones que muestran en pantalla

 * Para mostrar los eventos del día lo haremos en formato
 * timeline

 * Para mostrar los eventos de la semana los haremos por
 * día de la semana

 * Para mostrar los eventos del mes lo haremos en formato
 * de alamanaque

 * Para mostrar todos los eventos lo hacemos en formato de
 * tabla
-}
-------------------------------------------------------------
-------------------------------------------------------------

-- | Ordena los eventos
-- |
sortEvs :: [Event] -> [Event]
sortEvs = sortBy (comparing (fourth . startTime))

sortEvsDay :: [Event] -> [Event]
sortEvsDay = sortBy (comparing (first . startTime))

-- | Renderiza el cronograma del día completo en forma horizontal
-- |
renderTimeline ::[Event] -> Doc AnsiStyle
renderTimeline es = 
  let ses = sortEvs es
  in 
    vsep
      [ titleDoc "\nEventos de hoy:"
      , hardline
      , renderHour
      , vsep (map renderEvents ses)
      , hardline]

renderHour :: Doc AnsiStyle
renderHour = 
  annotate bold 
    (hcat 
      [pretty 
        (if h < 10 then "0" ++ show h
         else show h) <+> pretty " " | h <- [0 .. 24]])

renderEvents :: Event -> Doc AnsiStyle
renderEvents e =
  hcat 
    [renderBlock h e | h <- [0 .. 23]] <+> space <+> constDoc (summary e)

renderBlock :: Int -> Event -> Doc AnsiStyle
renderBlock h e =
  let evInH = fourth (startTime e) <= h && fourth (endTime e) > h
      block = if evInH
              then annotate (color Green) (pretty "████")
              else pretty "    "
  in block

timeline :: [Event] -> IO ()
timeline ev = putDoc $ renderTimeline ev

-- | Asociamos una fecha con un día de semana
-- | 
dayOfWeek :: DateTime -> String
dayOfWeek (DateTime (d,m,y,_,_)) =
  let (_,_,nd) = toWeekDate (fromGregorian (toInteger y) m d)
  in 
    case nd of
      1 -> "Lunes"
      2 -> "Martes"
      3 -> "Miércoles"
      4 -> "Jueves"
      5 -> "Viernes"
      6 -> "Sábado"
      7 -> "Domingo"
      _ -> "Error"

-- | Renderiza la visualización semanal
-- |
renderWeekly :: [Event] -> Doc AnsiStyle
renderWeekly es =
  let ses = sortEvs es
  in 
    vsep 
      [ titleDoc "\nEventos de la semana:"
      , hardline
      , vsep [renderDayEvents d ses | 
         d <- ["Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo"]]
      , hardline]

renderDayEvents :: String -> [Event] -> Doc AnsiStyle
renderDayEvents d es =
  let dayEvents = filter (\e -> dayOfWeek (startTime e) == d) es
      event = vcat 
        [constDoc (summary e) <+> 
         annotate bold (pretty "[") <>
         annotate bold (pretty (fourth (startTime e))) <>
         annotate bold (pretty ":") <>
         annotate bold (pretty (fifth (startTime e))) <+>
         annotate bold (pretty "-") <+>
         annotate bold (pretty (fourth (endTime e))) <>
         annotate bold (pretty ":") <>
         annotate bold (pretty (fifth (endTime e))) <>
         annotate bold (pretty "]") | e <- dayEvents]
  in annotate italicized (pretty d) <> 
     annotate italicized (pretty ":") <> 
     hardline <> event <> hardline

weekly :: [Event] -> IO ()
weekly ev = putDoc $ renderWeekly ev

-- | Obtiene el número de días de un mes
-- |
firstDay :: Int -> Int -> Int
firstDay y m = 
  let (_, _, d) = toWeekDate (fromGregorian (toInteger y) m 1)
  in d - 1

generateDays :: Int -> Int -> [Day]
generateDays y m =
  let year = toInteger y 
  in [fromGregorian year m d | d <- [1 .. gregorianMonthLength year m]]

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

-- | Renderiza la visualización mensual
-- |
renderMonthly :: Int -> Int -> [Event] -> Doc AnsiStyle
renderMonthly y m es =
  let ses = sortEvsDay es
  in 
    vsep 
      [ titleDoc "\nEventos del mes:"
      , hardline
      , annotate italicized (pretty "Lu  Ma  Mi  Ju  Vi  Sa  Do")
      , renderDays y m ses
      , hardline
      , annotate (italicized <> underlined) (pretty "Eventos:")
      , renderEvList ses
      , hardline]

renderDays :: Int -> Int -> [Event] -> Doc AnsiStyle
renderDays y m es = 
  let days = generateDays y m
      f = replicate (firstDay y m) "   "
      rows = chunk 7 (f ++ map (ppDay es) days)
  in vcat (map (annotate bold . pretty . unwords) rows)

ppDay :: [Event] -> Day -> String
ppDay es day = 
  let (y, m, d) = toGregorian day
      s = show d ++ if hasEvent es day then "*" else " "
  in alignR 3 s

hasEvent :: [Event] -> Day -> Bool
hasEvent es day = 
  let (y, m, d) = toGregorian day
  in any (\e -> first (startTime e) == d) es

alignR :: Int -> String -> String
alignR n s = s ++ replicate (n - length s) ' '

renderEvList :: [Event] -> Doc AnsiStyle
renderEvList = vsep . map renderEvent

renderEvent :: Event -> Doc AnsiStyle
renderEvent (Event s st et _ _ _) =
  annotate bold (pretty (first st)) <>
  annotate bold (pretty "/") <>
  annotate bold (pretty (second st)) <+>
  constDoc s

monthly :: [Event] -> IO ()
monthly ev = 
  let (y, m, _) = toGregorian (utctDay (unsafePerformIO getCurrentTime))
  in putDoc $ renderMonthly (fromInteger y) m ev


-- | Rodea un string con un string a la izquierda y otro a la derecha
-- |
encloseWith :: String -> String -> Doc AnsiStyle -> Doc AnsiStyle
encloseWith l r doc = pretty l <> doc <> pretty r

-- | Renderiza la visaulización de todos los eventos en formato de tabla
-- |
renderTable :: [Event] -> Doc AnsiStyle
renderTable es = 
  let ses = sortEvs es
      header = ["Event", "Start" , "End"]
      rows = map renderRow es
  in 
    vsep 
      [ titleDoc "\nTodos los eventos:"
      , hardline
      , pretty "|------------------------------------------------------------------------|"
      , renderHeader header
      , pretty "|------------------------------------------------------------------------|"
      , vsep rows
      , pretty "|------------------------------------------------------------------------|"
      , hardline ]

renderHeader :: [String] -> Doc AnsiStyle
renderHeader hdrs = 
  let h = hsep (map (encloseWith "| " " |" . annotate italicized . pretty . alignR 20) hdrs)
  in h

renderRow :: Event -> Doc AnsiStyle
renderRow (Event s st et _ _ _) =
  hsep
    [ encloseWith "| " " |" (constDoc (alignR 20 s))
    , encloseWith "| " " |" (
        annotate bold 
          (pretty (alignR 20
            (show (first st) ++ "/" ++ show (second st) ++ "/" ++ show (third st)
            ++ " " ++ show (fourth st) ++ ":" ++ show (fifth st)))))
    , encloseWith "| " " |" (
        annotate bold 
          (pretty (alignR 20
            (show (first et) ++ "/" ++ show (second et) ++ "/" ++ show (third et)
            ++ " " ++ show (fourth et) ++ ":" ++ show (fifth et))))) ]

table :: [Event] -> IO ()
table ev = putDoc $ renderTable ev
