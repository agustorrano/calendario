module CalendarOps where

import Data.Dates
import System.IO.Unsafe (unsafePerformIO)

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
eventExists e (x:xs) = if e == x then True 
                       else eventExists e xs

newEvent :: Event -> Calendar -> Either Error Calendar
newEvent e (Calendar u es) = case eventExists e es of
                               True -> Left Exists
                               False -> Right (Calendar u (e:es))

--------------------------------------------------------------------
--------------- Eliminamos un evento del calendario ----------------
--------------------------------------------------------------------
removeEvent :: Event -> [Event] -> [Event]
removeEvent _ [] = []
removeEvent e (x:xs) = if e == x then xs
                       else (x:(removeEvent e xs))

deleteEvent :: Event -> Calendar -> Either Error Calendar
deleteEvent e (Calendar u es) = case eventExists e es of
                                  False -> Left Unexists
                                  True -> Right (Calendar u (removeEvent e es))

--------------------------------------------------------------------
--------------- Modificamos un evento del calendario ---------------
--------------------------------------------------------------------
modifyTitle :: Event -> [Event] -> String -> [Event]
modifyTitle _ [] _ = []
modifyTitle e@(Event _ st et c r) (x:xs) t =
    if e == x then
        let ev = Event t st et c r
        in (ev:xs)
    else (x:(modifyTitle' e xs t))

modifyST :: Event -> [Event] -> DateTime -> [Event]
modifyST _ [] _ = []
modifyST e@(Event t _ et c r) (x:xs) st =
    if e == x then
        let ev = Event t st et c r
        in (ev:xs)
    else (x:(modifyST e xs st))

modifyET :: Event -> [Event] -> DateTime -> [Event]
modifyET _ [] _ = []
modifyET e@(Event t st _ c r) (x:xs) et =
    if e == x then
        let ev = Event t st et c r
        in (ev:xs)
    else (x:(modifyET e xs et))

modifyCategory :: Event -> [Event] -> Maybe Category -> [Event]
modifyCategory _ [] _ = []
modifyCategory e@(Event t st et _ r) (x:xs) c =
    if e == x then
        let ev = Event t st et c r
        in (ev:xs)
    else (x:(modifyCategory e xs c))

modifyRec :: Event -> [Event] -> Maybe Recurrence -> [Event]
modifyRec _ [] _ = []
modifyRec e@(Event t st et c _) (x:xs) r =
    if e == x then
        let ev = Event t st et c r
        in (ev:xs)
    else (x:(modifyRec e xs r))

--------------------------------------------------------------------
--------------- Buscamos un evento en el calendario ----------------
--------------------------------------------------------------------
searchEvent :: String -> [Event] -> Either Error [Event]
searchEvent _ [] = Left Unexists
searchEvent s (x:xs) = 
    if s == summary x then Right (x:(searchEvent s xs))
    else searchEvent s xs

--------------------------------------------------------------------
------------------ Obtenemos los eventos del día -------------------
--------------------------------------------------------------------
isInDay :: Event -> Bool
isInDay (ENoCat (EventWithoutCat _ t _)) = (unsafePerformIO getCurrentDateTime) == t
isInDay (ECat (EventWithCat _ t _ _)) = (unsafePerformIO getCurrentDateTime) == t

thisDay :: Calendar -> [Event]
thisDay cal = thisDay' cal []

thisDay' :: Calendar -> [Event] -> [Event]
thisDay' (Calendar u []) xs = xs
thisDay' (Calendar u (e:es)) xs = if isInDay e then thisDay' (Calendar u es) (e:xs)
                                  else thisDay' (Calendar u es) xs

--------------------------------------------------------------------
---------------- Obtenemos los eventos de la semana ----------------
--------------------------------------------------------------------
-- función ue obtiene el lunes de la semana actual
getMonday :: DateTime -> DateTime
getMonday d = let untilMonday = mod ((fromEnum (dateWeekDay d)) + 6) 7
              in addInterval d (Days (-(fromIntegral untilMonday)))

getSunday :: DateTime -> DateTime
getSunday monday = addInterval monday (Days 6)

isInWeek :: Event -> Bool
isInWeek (ENoCat (EventWithoutCat _ t _)) = let monday = getMonday (unsafePerformIO getCurrentDateTime)
                                                sunday = getSunday monday
                                            in (t >= monday && t <= sunday)
isInWeek (ECat (EventWithCat _ t _ _)) = let monday = getMonday (unsafePerformIO getCurrentDateTime)
                                             sunday = getSunday monday
                                         in (t >= monday && t <= sunday)

thisWeek :: Calendar -> [Event]
thisWeek cal = thisWeek' cal []

thisWeek' :: Calendar -> [Event] -> [Event]
thisWeek' (Calendar u []) xs = xs
thisWeek' (Calendar u (e:es)) xs = if isInWeek e then thisWeek' (Calendar u es) (e:xs)
                                   else thisWeek' (Calendar u es) xs

--------------------------------------------------------------------
------------------ Obtenemos los eventos del mes -------------------
--------------------------------------------------------------------
-- función para obtener el primer día del mes
getStartOfMonth :: DateTime -> DateTime
getStartOfMonth d = d {day = 1}

getEndOfMonth :: DateTime -> DateTime
getEndOfMonth startOfMonth = addInterval (addInterval startOfMonth (Months 1)) (Days (-1))

isInMonth :: Event -> Bool
isInMonth (ENoCat (EventWithoutCat _ t _)) = let startOfMonth = getStartOfMonth (unsafePerformIO getCurrentDateTime)
                                                 endOfMonth = getEndOfMonth startOfMonth
                                             in (t >= startOfMonth && t <= endOfMonth)
isInMonth (ECat (EventWithCat _ t _ _)) = let startOfMonth = getStartOfMonth (unsafePerformIO getCurrentDateTime)
                                              endOfMonth = getEndOfMonth startOfMonth
                                          in (t >= startOfMonth && t <= endOfMonth)

thisMonth :: Calendar -> [Event]
thisMonth cal = thisMonth' cal []

thisMonth' :: Calendar -> [Event] -> [Event]
thisMonth' (Calendar u []) xs = xs
thisMonth' (Calendar u (e:es)) xs = if isInMonth e then thisMonth' (Calendar u es) (e:xs)
                                    else thisMonth' (Calendar u es) xs

--------------------------------------------------------------------
----------- Obtenemos todos los eventos de un calendario -----------
--------------------------------------------------------------------
allEvents :: Calendar -> [Event]
allEvents (Calendar u es) = es

--------------------------------------------------------------------
-------- Obtenemos todos los eventos de la misma categoría ---------
--------------------------------------------------------------------
category :: [Event] -> Category -> [Event]
category [] _ = []
category (x:xs) cat = if cat == category x
                      then (x:(category xs cat))
                      else category xs cat
