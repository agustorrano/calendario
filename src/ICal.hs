module ICal where

import ICal.Org
import Data.Time
import Data.Text (pack)
import qualified Data.Text.IO as TIO
import Data.UUID (UUID, toString)
import Data.UUID.V4 (nextRandom)

import Common

-- Función que genera un UUID aleatorio
getUID :: IO String
getUID = do uid <- nextRandom
         return $ toString uid

eventToICal :: Event -> String
eventToICal (ENoCat (EventWithoutCat s st et)) = 
  "BEGIN:VEVENT\n" ++
  "UID:" ++ getUID ++ "\n" ++
  "DTSTART:" ++ st ++ "\n" ++
  "DTEND:" ++ et ++ "\n" ++
  "SUMMARY:" ++ s ++ "\n" ++
  "END:VEVENT\n"
eventToICal (ECat (EventWithCat s st et c)) =
  "BEGIN:VEVENT\n" ++
  "UID:" ++ getUID ++ "\n" ++
  "DTSTART:" ++ st ++ "\n" ++
  "DTEND:" ++ et ++ "\n" ++
  "SUMMARY:" ++ s ++ "\n" ++
  "CATEGORY:" ++ c ++ "\n" ++
  "END:VEVENT\n"


exportToICal :: FilePath -> [Event] -> IO ()
exportToICal file events = do
  let hdr = "BEGIN:VCALENDAR\n-//Google Inc//Google Calendar 70.9054//EN\nVERSION:2.0\n"
      end = "END:VCALENDAR\n"
      body = concatMap eventToICal events
      icalFile = hdr ++ body ++ body
  in writeFile file icalFile
      