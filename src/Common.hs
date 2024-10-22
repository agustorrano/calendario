-- | Tipos de datos utilizados
module Common where

type Category = String
type Name = String

newtype DateTime = 
  DateTime (Int, Int, Int, Int, Int)
  deriving (Show, Eq)

instance Ord DateTime where
  DateTime (d1,m1,y1,_,_) <= DateTime (d2,m2,y2,_,_) = d1 <= d2 && m1 <= m2 && y1 <= y2
  DateTime (d1,m1,y1,_,_) >= DateTime (d2,m2,y2,_,_) = d1 >= d2 && m1 >= m2 && y1 >= y2

data Recurrence = 
    Daily Int
  | Weekly Int
  | Monthly Int
  deriving (Show, Eq)

data Event = 
  Event { summary :: String
        , startTime :: DateTime
        , endTime :: DateTime
        , category :: Maybe Category
        , recurrence :: Maybe Recurrence
        , holeDay :: Bool
        } deriving (Show, Eq)

data Calendar = Calendar Name [Event] | Null deriving Show

data Error = Exists | Unexists | NoRec deriving Show

data CompileForm = 
    CompileInteractive  String
  | CompileFile         String
  deriving Show

data CalCom = 
    NewCalendar Name
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

data InterCom = 
    Compile String
  | Reload
  | Print
  | Quit
  | Help
  | Noop
  | Ops
  | Close
  | Export
  | Import String
  deriving Show

data Command = CCom CalCom | ICom InterCom

data InteractiveCommand = Cmd [String] String (String -> InterCom) String

data CalendarCommand = CCmd String [String] String
