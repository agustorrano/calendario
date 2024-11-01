-- | Tipos de datos utilizados
module Common where

type Category = String
type Name = String

newtype DateTime = 
  DateTime (Int, Int, Int, Int, Int)
  deriving (Show)

instance Eq DateTime where
  (DateTime (d1,m1,y1,h1,mi1)) == (DateTime (d2,m2,y2,h2,mi2)) =
    d1 == d2 && m1 == m2 && y1 == y2 && h1 == h2 && mi1 == mi2

instance Ord DateTime where
  compare (DateTime (d1,m1,y1,_,_)) (DateTime (d2,m2,y2,_,_)) =
    compare (y1,m1,d1) (y2,m2,d2)

data Recurrence = 
    Daily Int
  | Weekly Int
  | Monthly Int
  deriving (Show, Eq)

data Event = 
  Event 
    { summary :: String
    , startTime :: DateTime
    , endTime :: DateTime
    , category :: Maybe Category
    , recurrence :: Maybe Recurrence
    , holeDay :: Bool
    } deriving (Show)

instance Eq Event where
  (Event s1 st1 et1 c1 r1 b1) == (Event s2 st2 et2 c2 r2 b2) =
    s1 == s2 && st1 == st2 && et1 == et2

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
