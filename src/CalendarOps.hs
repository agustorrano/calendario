module CalendarOps where

import Data.Dates
-- import System.IO.Unsafe (unsafePerformIO)

import Common

newCalendar :: Name -> Calendar
newCalendar n = Calendar n []

-- tendría q ver que uso para el evento
-- por ahi puedo generar distintos eventos
-- este sería el básico
newEvent :: String -> DateTime -> DateTime -> Maybe Category -> Event
newEvent s st et Nothing = ENoCat (EventWithoutCat s st et)
newEvent s st et (Just c) = ECat (EventWithCat s st et c)

--------------------------------------------------------------------
---------------- Agregamos un evento al calendario -----------------
--------------------------------------------------------------------
eventExists :: Event -> [Event] -> Bool
eventExists _ [] = False
eventExists e (x:xs) = if r == x then True 
                       else eventExists e xs

addEvent :: Event -> Calendar -> Either Error Calendar
addEvent e (Calendar u es) = case eventExists e es of
                               True -> Left Exists
                               Flase -> Right (Calendar u (e:es))


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
                                  True -> Calendar u (removeEvent e es)

--------------------------------------------------------------------
------------------ Obtenemos los eventos del día -------------------
--------------------------------------------------------------------
isInDay :: Event -> Bool
isInDay (Event d t c) = getCurrentDayTime == t

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
getMonday d = let untilMonday = mod (dateWeekDay d + 6) 7
              in addInterval d (Days (-untilMonday))

getSunday :: DateTime -> DateTime
getSunday monday = addInterval monday (Days 6)

isInWeek :: Event -> Bool
isInWeek (Event d t c) = let monday = getMonday getCurrentDayTime
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
isInMonth (Event d t c) = let startOfMonth = getStartOfMonth getCurrentDayTime
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