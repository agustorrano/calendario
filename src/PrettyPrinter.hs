module PrettyPrinter where

import Common
import Text.PrettyPrint.HughesPJ
import Prelude hiding ( (<>) )
import Data.Dates

-- Imprime una fecha
printDate :: DateTime -> Doc
printDate date = 
  let d = day date
      m = month date
      y = year date
      h = hour date
      mi = minute date
  in int d <> text "/" <> int m <> text "/"
     <> int y <+> int h <> colon <> int mi

-- Imprime un evento
printEvent :: Event -> Doc
printEvent (ENoCat (EventWithoutCat s st et)) = 
  text "Event" <+> text s <+> printDate st <+> printDate et
printEvent (ECat (EventWithCat s st et c)) =
  text "Event" <+> text s <+> printDate st <+> printDate et <+> text c

-- Imprime una lista
printList :: [a] -> (a -> Doc) -> Doc
printList [] _ = empty
printList [x] p = p x <> text "\n"
printList (x:xs) p = p x <> comma <> text "\n" 
                     <> (printList xs p)

-- Imprime una lista de eventos
printListEvent :: [Event] -> Doc
printListEvent es = brackets (printList es printEvent)

-- Imprime un calndario
printCal :: Calendar -> Doc
printCal (Calendar u es) = text "Cal" <+> text u <> text "\n"
                           <> printListEvent es
