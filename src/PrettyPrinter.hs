module PrettyPrinter where

import Common
import Data.Text ( unpack )
import Prettyprinter.Render.Terminal
  ( renderStrict, italicized, bold, color, colorDull, Color (..), AnsiStyle )
import Prettyprinter
  ( (<+>),
    annotate,
    defaultLayoutOptions,
    layoutSmart,
    nest,
    line,
    comma,
    sep,
    parens,
    brackets,
    Doc,
    Pretty(pretty) )

-- Color para el encabezado
constColor :: Doc AnsiStyle -> Doc AnsiStyle
constColor = annotate (bold <> colorDull Cyan)

-- Color para el separador
sepColor :: Doc AnsiStyle -> Doc AnsiStyle
sepColor = annotate (bold <> color Magenta)

-- Color para el título del evento
keywordColor :: Doc AnsiStyle -> Doc AnsiStyle
keywordColor = annotate (italicized <> color Green)

-- Color para la fecha y horario
timeColor :: Doc AnsiStyle -> Doc AnsiStyle
timeColor = annotate (color Blue)

-- Color para el resto
nameColor :: Doc AnsiStyle -> Doc AnsiStyle
nameColor = id

intDoc :: Int -> Doc AnsiStyle
intDoc n = constColor (pretty (show n))

sepDoc :: String -> Doc AnsiStyle
sepDoc s = sepColor (pretty s)

keywordDoc :: String -> Doc AnsiStyle
keywordDoc s = keywordColor (pretty s)

nameDoc :: String -> Doc AnsiStyle
nameDoc s = nameColor (pretty s)

-- Imprime una fecha
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

-- Imprime un evento
printEvent :: Event -> Doc AnsiStyle
printEvent (Event s st et Nothing r b) = 
  keywordDoc "Event" <+> nameDoc s <+> printDate st <+> printDate et
printEvent (Event s st et (Just c) r b) =
  keywordDoc "Event" <+> nameDoc s <+> printDate st <+> printDate et <+> nameDoc c

-- Imprime una lista
printList :: [a] -> (a -> Doc AnsiStyle) -> Doc AnsiStyle
printList [] _ = mempty
printList [x] p = p x <> line
printList (x:xs) p = 
  p x <> comma <> line <> printList xs p

-- Imprime una lista de eventos
printListEvent :: [Event] -> Doc AnsiStyle
printListEvent es = printList es printEvent

-- Imprime un calndario
printCal :: Calendar -> Doc AnsiStyle
printCal (Calendar u es) = 
  keywordDoc "Cal" <+> nameDoc u <> line <> printListEvent es

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

ppListEv :: [Event] -> String
ppListEv = render . printListEvent