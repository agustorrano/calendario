-- Tipos de datos utilizados
module Common where

import Data.Dates

type Category = String
type Name = String

data Recurrence = Daily Int
                | Weekly Int
                | Monthly Int
                deriving (Show, Eq)

data Event = Event { summary :: String
                   , startTime :: DateTime
                   , endTime :: DateTime
                   , category :: Maybe Category
                   , recurrence :: Maybe Recurrence
                   } deriving (Show, Eq)

data Calendar = Calendar Name [Event] | Null deriving Show

data Error = Exists | Unexists deriving Show

data CompileForm = CompileInteractive  String
                 | CompileFile         String
                 deriving Show

data CalCom = NewCalendar Name
            | NewEvent Event
            | ModifyEvent Event
            | DeleteEvent Event
            | SearchEvent String
            | ThisDay
            | ThisWeek
            | ThisMonth
            | AllEvents 
            | Category Category
            deriving Show

data InterCom = Compile String
              | Recompile
              | Quit
              | Help
              | Noop
              | Ops
              | Close 
              | Export String 
              deriving Show

data Command = CCom CalCom | ICom InterCom

data InteractiveCommand = Cmd [String] String (String -> InterCom) String

data CalendarCommand = CCmd String [String] String
