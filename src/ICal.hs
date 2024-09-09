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
  uid <- nextRandom
  return $ toString uid

eventToICal :: Event -> IO String
eventToICal (ENoCat (EventWithoutCat s st et)) = do
  uid <- getUID
  -- let sTime = showDateTime st
  -- let eTime = showDateTime et
  return $
    "BEGIN:VEVENT\n" ++
    "UID:" ++ uid ++ "\n" ++
    "DTSTART:" ++ (show st) ++ "\n" ++
    "DTEND:" ++ (show et) ++ "\n" ++
    "SUMMARY:" ++ s ++ "\n" ++
    "END:VEVENT\n"
eventToICal (ECat (EventWithCat s st et c)) = do
  uid <- getUID
  -- let sTime = showDateTime st
  -- let eTime = showDateTime et
  return $
    "BEGIN:VEVENT\n" ++
    "UID:" ++ uid ++ "\n" ++
    "DTSTART:" ++ (show st) ++ "\n" ++
    "DTEND:" ++ (show et) ++ "\n" ++
    "SUMMARY:" ++ s ++ "\n" ++
    "CATEGORY:" ++ c ++ "\n" ++
    "END:VEVENT\n"


exportToICal :: FilePath -> Calendar -> IO ()
exportToICal file (Calendar n events) = do
  eventStrings <- mapM eventToICal events
  let hdr = "BEGIN:VCALENDAR\n-//Google Inc//Google Calendar 70.9054//EN\nVERSION:2.0\n"
      end = "END:VCALENDAR\n"
      body = concat eventStrings
      icalFile = hdr ++ body ++ body
  writeFile file icalFile
      