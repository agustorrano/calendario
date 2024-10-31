module ICS where

import Data.List as L
import Data.Text as T
import Data.Time.Calendar ( gregorianMonthLength )
import Data.Maybe

import Common

-- | Suma 3 horas
-- | Es necesario para pasarlo a Google Calendar
-- |
suma3 :: DateTime -> DateTime
suma3 (DateTime (d, m, y, h, min)) = 
  let sum = h + 3
      (sDay, h') = divMod sum 24
      daysInMonth = gregorianMonthLength (toInteger y) m
      nd = d + sDay
      (d', m', y') = adjustDate nd m y daysInMonth
  in DateTime (d', m', y', h', min)

-- | Ajusta el día, mes y año según sea necesario
-- |
adjustDate :: Int -> Int -> Int -> Int -> (Int, Int, Int)
adjustDate d m y daysInMonth
  | d > daysInMonth = (1, nextM, nextY)
  | otherwise = (d, m, y)
  where nextM = if m == 12 then 1 else m + 1
        nextY = if m == 12 then y + 1 else y

-- | Convierte un DateTime en un string con el formato para un archivo .ical
-- |
dt2str :: DateTime -> String
dt2str (DateTime (d, m, y, h, min)) =
  let aux n = if n < 10 then '0':show n else show n
  in aux y ++ aux m ++ aux d ++ "T" ++ aux h ++ aux min ++ "00Z"

dt2str' :: DateTime-> String
dt2str' (DateTime (d, m, y, _, _)) =
  let aux n = if n < 10 then '0':show n else show n
  in aux y ++ aux m ++ aux d

-- | Convierte un evento en un formato .ical
-- |
event2ics :: Event -> String
event2ics (Event s st et c r b) = 
  if b then 
    let str =  L.unlines ["BEGIN:VEVENT"
          , "DTSTART;VALUE=DATE:" ++ dt2str' st
          , "DTEND;VALUE=DATE:" ++ dt2str' et
          , "UID:" ++ s ++ dt2str st
          , "STATUS:CONFIMED"
          , "SUMMARY:" ++ s
          ]
    in str ++ "END:VEVENT"
  else 
    let str = L.unlines ["BEGIN:VEVENT"
          , "DTSTART:" ++ dt2str (suma3 st)
          , "DTEND:" ++ dt2str (suma3 et)
          , "UID:" ++ s ++ dt2str st
          , "STATUS:CONFIMED"
          , "SUMMARY:" ++ s
          ]
    in str ++ "END:VEVENT"

-- | Exporta un calendario a un archivo .ics
-- |
exportToIcs :: Calendar -> IO ()
exportToIcs (Calendar n es) = do
  let content = L.unlines ["BEGIN:VCALENDAR"
        , "PRODID:-//Google Inc//Google Calendar 70.9054//EN"
        , "VERSION:2.0","CALSCALE:GREGORIAN"
        , "METHOD:PUBLISH"
        , "X-WR-TIMEZONE:America/Argentina/Cordoba"
        , L.intercalate "\n" (L.map event2ics es)
        , "END:VCALENDAR"]
      file = n
  writeFile (file ++ ".ics") content
  putStrLn $ "Calendario exportado a: " ++ (file ++ ".ics")

-- | Resta 3 horas
-- |
resta3 :: DateTime -> DateTime
resta3 (DateTime (d, m, y, h, min)) = 
  let res = h - 3
      (rDay, h') = if res < 0 then (-1, res + 24) else (0, res)
      (d', m', y') = adjustDateRes (d + rDay) m y
  in DateTime (d', m', y', h', min)

-- | Ajusta el día, mes y año según sea necesario
-- |
adjustDateRes :: Int -> Int -> Int -> (Int, Int, Int)
adjustDateRes d m y
  | d < 1 = (gregorianMonthLength (toInteger prevY) prevM, prevM, prevY)
  | otherwise = (d, m, y)
  where prevM = if m == 1 then 12 else m - 1
        prevY = if m == 1 then y - 1 else y

-- | Convierte un string en un DateTime
-- |
str2dt :: String -> DateTime
str2dt str = 
  let (y, r1) = L.splitAt 4 str
      (m, r2) = L.splitAt 2 r1
      (d, r3) = L.splitAt 2 r2
      (h, r4) = L.splitAt 2 (L.drop 1 r3)
      (min, _) = L.splitAt 2 r4
  in DateTime (read d, read m, read y, read h, read min)

str2dt' :: String -> DateTime
str2dt' str = 
  let (y, r1) = L.splitAt 4 str
      (m, r2) = L.splitAt 2 r1
      (d, _) = L.splitAt 2 r2
  in DateTime (read d, read m, read y, 0, 0)

-- | Convierte un string en una tupla clave valor
-- |
str2tuple :: String -> Maybe (String, String)
str2tuple str =
  case L.break (== ':') str of
    (k, ':' : val) -> Just (k, val)
    _ -> Nothing

-- | Busca una calve en una lista de tuplas
-- |
lookupKey :: String -> [(String, String)] -> Maybe String
lookupKey k = fmap snd . L.find ((== k) . fst)

parseHoleDay :: [String] -> Maybe Event
parseHoleDay str = do
  let ps = mapMaybe str2tuple str
  dst <- lookupKey "DTSTART;VALUE=DATE" ps
  det <- lookupKey "DTEND;VALUE=DATE" ps
  s <- lookupKey "SUMMARY" ps
  let st = str2dt' dst
      et = str2dt' det
  return $ Event s st et Nothing Nothing True

-- | Parsea una evento en formato .ical
-- |
parseICal :: [String] -> Maybe Event
parseICal str = do
  let ps = mapMaybe str2tuple str
  dst <- lookupKey "DTSTART" ps
  det <- lookupKey "DTEND" ps
  s <- lookupKey "SUMMARY" ps
  let st = str2dt dst
      et = str2dt det
  return $ Event s (resta3 st) (resta3 et) Nothing Nothing False

-- | Divide el calendario en bloques de eventos
-- |
splitEv :: [String] -> [[String]]
splitEv es =
  let txt = L.map T.pack es
      block = T.splitOn (T.pack "END:VEVENT") (T.unlines txt)
  in L.map (L.map unpack . T.lines) block

-- | Importa un .ics
-- |
importIcs :: FilePath -> Calendar -> IO Calendar
importIcs file (Calendar n e) = do
  cont <- fmap (L.map (L.filter (/= '\r')) . L.lines) (readFile file)
  let es = mapMaybe parseICal (splitEv cont)
      es' = mapMaybe parseHoleDay (splitEv cont)
  return $ Calendar n (es ++ es' ++ e)

