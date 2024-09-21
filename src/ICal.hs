module ICal where

import Data.Time
import Data.Text (pack)
import qualified Data.Text.IO as TIO
import Data.UUID (UUID, toString)
import Data.UUID.V4 (nextRandom)

import Common
import Parse

-- Función que genera un UUID aleatorio
getUID :: IO String
getUID = do
  toString <$> nextRandom

eventToICal :: Event -> IO String
eventToICal (Event s st et c r b) = do
  uid <- getUID
  case c of
    Nothing -> return $
      "BEGIN:VEVENT\n" ++
      "UID:" ++ uid ++ "\n" ++
      "DTSTART:" ++ show st ++ "\n" ++
      "DTEND:" ++ show et ++ "\n" ++
      "SUMMARY:" ++ s ++ "\n" ++
      "END:VEVENT\n"
    Just cat -> return $
      "BEGIN:VEVENT\n" ++
      "UID:" ++ uid ++ "\n" ++
      "DTSTART:" ++ show st ++ "\n" ++
      "DTEND:" ++ show et ++ "\n" ++
      "SUMMARY:" ++ s ++ "\n" ++
      "CATEGORY:" ++ cat ++ "\n" ++
      "END:VEVENT\n"

exportToICal :: FilePath -> Calendar -> IO ()
exportToICal file (Calendar n events) = do
  eventStrings <- mapM eventToICal events
  let hdr = "BEGIN:VCALENDAR\n-//Google Inc//Google Calendar 70.9054//EN\nVERSION:2.0\n"
      end = "END:VCALENDAR\n"
      body = concat eventStrings
      icalFile = hdr ++ body ++ body
  writeFile file icalFile
