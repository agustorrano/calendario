module CalendarOps where

import System.IO.Unsafe (unsafePerformIO)
import Data.Time
import Data.Time.Calendar.OrdinalDate (mondayStartWeek
                                      , fromMondayStartWeek)

import Common

--------------------------------------------------------------------
------------------- Creamos un nuevo calendario --------------------
--------------------------------------------------------------------
newCalendar :: Name -> Calendar
newCalendar n = Calendar n []

--------------------------------------------------------------------
--------------------- Creamos un nuevo evento ----------------------
--------------------------------------------------------------------
eventExists :: Event -> [Event] -> Bool
eventExists _ [] = False
eventExists e (x:xs) = (e == x) || eventExists e xs

newEvent :: Event -> Calendar -> Either Error Calendar
newEvent e (Calendar u es) = 
    if eventExists e es then Left Exists 
    else Right (Calendar u (e:es))

--------------------------------------------------------------------
--------------- Eliminamos un evento del calendario ----------------
--------------------------------------------------------------------
removeEvent :: Event -> [Event] -> [Event]
removeEvent _ [] = []
removeEvent e (x:xs) = if e == x then xs
                       else x:removeEvent e xs

deleteEvent :: Event -> Calendar -> Either Error Calendar
deleteEvent e (Calendar u es) = 
    if eventExists e es then Right (Calendar u (removeEvent e es)) 
    else Left Unexists

--------------------------------------------------------------------
--------------- Modificamos un evento del calendario ---------------
--------------------------------------------------------------------
modifyTitle :: Event -> [Event] -> String -> [Event]
modifyTitle _ [] _ = []
modifyTitle e@(Event _ st et c r b) (x:xs) t =
    if e == x then
        let ev = Event t st et c r b
        in (ev:xs)
    else x:modifyTitle e xs t

modifyST :: Event -> [Event] -> DateTime -> [Event]
modifyST _ [] _ = []
modifyST e@(Event t _ et c r b) (x:xs) st =
    if e == x then
        let ev = Event t st et c r b
        in (ev:xs)
    else x:modifyST e xs st

modifyET :: Event -> [Event] -> DateTime -> [Event]
modifyET _ [] _ = []
modifyET e@(Event t st _ c r b) (x:xs) et =
    if e == x then
        let ev = Event t st et c r b
        in (ev:xs)
    else x:modifyET e xs et

modifyCategory :: Event -> [Event] -> Maybe Category -> [Event]
modifyCategory _ [] _ = []
modifyCategory e@(Event t st et _ r b) (x:xs) c =
    if e == x then
        let ev = Event t st et c r b
        in (ev:xs)
    else x:modifyCategory e xs c

modifyRec :: Event -> [Event] -> Maybe Recurrence -> [Event]
modifyRec _ [] _ = []
modifyRec e@(Event t st et c _ b) (x:xs) r =
    if e == x then
        let ev = Event t st et c r b
        in (ev:xs)
    else x:modifyRec e xs r

--------------------------------------------------------------------
--------------- Buscamos un evento en el calendario ----------------
--------------------------------------------------------------------
searchEvent :: String -> [Event] -> [Event]
searchEvent _ [] = []
searchEvent s (x:xs) =
    if s == summary x then x:searchEvent s xs
    else searchEvent s xs

--------------------------------------------------------------------
------------ Obtenemos los eventos según un predicado --------------
--------------------------------------------------------------------

getEvents :: Calendar -> [Event] -> (Event -> Bool) -> [Event]
getEvents (Calendar _ []) xs _ = xs
getEvents (Calendar u (e:es)) xs f = if f e then getEvents (Calendar u es) (e:xs) f
                                     else getEvents (Calendar u es) xs f

--------------------------------------------------------------------
------------------ Obtenemos los eventos del día -------------------
--------------------------------------------------------------------
isInDay :: Event -> Bool
isInDay (Event _ (DateTime (d, m, y, _, _)) _ _ _ _) = 
    let (year, mon, day) = toGregorian (utctDay (unsafePerformIO getCurrentTime)) 
    in fromInteger year == y && mon == m && day == d

thisDay :: Calendar -> [Event]
thisDay cal = getEvents cal [] isInDay

--------------------------------------------------------------------
---------------- Obtenemos los eventos de la semana ----------------
--------------------------------------------------------------------
-- función ue obtiene el lunes y domingo de la semana actual
getMondayandSunday :: Day -> (DateTime, DateTime)
getMondayandSunday date = 
    let (y, m, d) = toGregorian date
        (week, i) = mondayStartWeek date
        monday = fromMondayStartWeek (toInteger y) week 1
        sunday = addDays 6 monday
        (ym, mm, dm) = toGregorian monday
        (ys, ms, ds) = toGregorian sunday
    in (DateTime (dm, mm, fromInteger ym, 0, 0), DateTime (ds, ms, fromInteger ys, 0, 0))

isInWeek :: Event -> Bool
isInWeek (Event _ t _ _ _ _) = 
    let (monday, sunday) = getMondayandSunday (utctDay (unsafePerformIO getCurrentTime))
    in t >= monday && t <= sunday

thisWeek :: Calendar -> [Event]
thisWeek cal = getEvents cal [] isInWeek

--------------------------------------------------------------------
------------------ Obtenemos los eventos del mes -------------------
--------------------------------------------------------------------
-- función para obtener el primer día del mes
getStartOfMonth :: DateTime -> DateTime
getStartOfMonth (DateTime (_, m, y, _, _)) = DateTime (1, m, y, 0, 0)

getEndOfMonth :: DateTime -> DateTime
getEndOfMonth (DateTime (_, m, y, _, _)) =
    let firstNextMonth = fromGregorian (toInteger y) (m + 1) 1
        last = addDays (-1) firstNextMonth
        (year, mon, d) = toGregorian last
    in DateTime (d, mon, fromInteger year, 0, 0)

isInMonth :: Event -> Bool
isInMonth (Event _ t _ _ _ _) = 
    let (y, m, d) = toGregorian (utctDay (unsafePerformIO getCurrentTime))
        startOfMonth = getStartOfMonth (DateTime (d, m, fromInteger y, 0, 0))
        endOfMonth = getEndOfMonth startOfMonth
    in (t >= startOfMonth && t <= endOfMonth)

thisMonth :: Calendar -> [Event]
thisMonth cal = getEvents cal [] isInMonth

--------------------------------------------------------------------
----------- Obtenemos todos los eventos de un calendario -----------
--------------------------------------------------------------------
allEvents :: Calendar -> [Event]
allEvents (Calendar u es) = es

--------------------------------------------------------------------
-------- Obtenemos todos los eventos de la misma categoría ---------
--------------------------------------------------------------------
sameCategory :: [Event] -> Category -> [Event]
sameCategory [] _ = []
sameCategory (x:xs) cat = case category x of
    Nothing -> sameCategory xs cat
    Just c -> if cat == c then x:sameCategory xs cat
              else sameCategory xs cat

--------------------------------------------------------------------
---------------- AGREGAR OPERACIONES DE RECURRENCIA ----------------
--------------------------------------------------------------------