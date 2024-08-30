-- Tipos de datos utilizados
module Common where

import Data.Dates

type Category = String
type Name = String

data EventWithCat = EventWithCat { summary1 :: String
                                 , startTime1 :: DateTime
                                 , endTime1 :: DateTime
                                 , category :: Category
                                 } deriving (Show, Eq)

data EventWithoutCat = EventWithoutCat { summary2 :: String
                                       , startTime2 :: DateTime
                                       , endTime2 :: DateTime
                                       } deriving (Show, Eq)

data Event = ENoCat EventWithoutCat
           | ECat EventWithCat 
           deriving (Show, Eq)

data Calendar = Calendar Name [Event] deriving Show

data Error = Exists | Unexists deriving Show

data CompileForm = CompileInteractive  String
                 | CompileFile         String
                 deriving Show

data CalCom = NewCalendar Name
            | NewEvent String DateTime DateTime (Maybe Category)
            | AddEvent Event
            | DeleteEvent Event
            | ThisDay
            | ThisWeek
            | ThisMonth
            | AllEvents 
            deriving Show

data InterCom = Compile CompileForm
              | Recompile
              | Print String
              | Quit
              | Help
              | Noop
              | Ops
              | Save
              | Close 
              | Export String 
              | Import String 
              deriving Show

data Command = CCom CalCom | ICom InterCom

data InteractiveCommand = Cmd [String] String (String -> InterCom) String

data CalendarCommand = CCmd String [String] String
