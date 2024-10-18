module RecurrenceOps where

import Data.Time
  ( fromGregorian
  , addDays
  , diffDays
  , addGregorianMonthsClip
  , Day, toGregorian )

import Common

expandEvent :: Event -> [Event]
expandEvent ev@(Event s st et c (Just r) b) =
  case r of
    Daily n -> createEvents ev n (addDays . toInteger)
    Weekly n -> createEvents ev (n * 7) (addDays . toInteger)
    Monthly n -> createEvents ev n (addGregorianMonthsClip . toInteger)
expandEvent ev = [ev]

createEvents :: Event -> Int -> (Int -> Day -> Day) -> [Event]
createEvents ev n f = take 10 $ iterate (moveEvent f n) ev

moveEvent :: (Int -> Day -> Day) -> Int -> Event -> Event
moveEvent f n (Event s (DateTime (d1,m1,y1,h1,mi1)) (DateTime (d2,m2,y2,h2,mi2)) c r b) =
  let day1 = fromGregorian (toInteger y1) m1 d1
      day2 = fromGregorian (toInteger y2) m2 d2
      nday1 = f n day1
      nday2 = f n day2
      (y1',m1',d1') = toGregorian nday1
      (y2',m2',d2') = toGregorian nday2
  in Event s (DateTime (d1',m1',fromInteger y1',h1,mi1)) 
     (DateTime (d2',m2',fromInteger y2',h2,mi2)) c r b
