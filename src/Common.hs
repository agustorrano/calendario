-- Tipos de datos utilizados
module Common where

import Data.Dates

import Parse

type Category = String
type Name = String

data EventWithCat = EventWithCat { summary :: String
                                 , startTime :: DateTime
                                 , endTime :: DateTime
                                 , category :: Category
                                 } deriving Show

data EventWithoutCat = EventWithoutCat { summary :: String
                                       , startTime :: DateTime
                                       , endTime :: DateTime
                                       } deriving Show

data Event = ENoCat EventWithoutCat
           | ECat EventWithCat deriving Show

data Calendar = Calendar [Event]

data Error = Exists | Unexists
