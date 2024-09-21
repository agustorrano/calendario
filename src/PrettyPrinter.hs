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
    sep,
    parens,
    Doc,
    Pretty(pretty) )
-- import Text.PrettyPrint.HughesPJ
-- import Prelude hiding ( (<>) )

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

-- Imprime una fecha
first :: DateTime -> Int
first (d, _, _, _, _) = d

second :: DateTime -> Int
second (_, m, _, _, _) = m

third :: DateTime -> Int
third (_, _, y, _, _) = y

fourth :: DateTime -> Int
fourth (_, _, _, h, _) = h

fifth :: DateTime -> Int
fifth (_, _, _, _, m) = m

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
  keywordDoc "Event" <+> text s <+> printDate st <+> printDate et
printEvent (Event s st et (Just c) r b) =
  text "Event" <+> text s <+> printDate st <+> printDate et <+> text c

-- Imprime una lista
printList :: [a] -> (a -> Doc AnsiStyle) -> Doc AnsiStyle
printList [] _ = empty
printList [x] p = p x <> text "\n"
printList (x:xs) p = p x <> comma <> text "\n" 
                     <> printList xs p

-- Imprime una lista de eventos
printListEvent :: [Event] -> Doc AnsiStyle
printListEvent es = brackets (printList es printEvent)

-- Imprime un calndario
printCal :: Calendar -> Doc AnsiStyle
printCal (Calendar u es) = text "Cal" <+> text u <> text "\n"
                           <> printListEvent es
