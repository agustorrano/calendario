module Parse where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language ( emptyDef )
import Text.Parsec.Char
import Data.Dates
import Data.Char
import Common

------------------------------------

lisComm :: TokenParser p
lisComm = makeTokenParser 
  (emptyDef 
    { commentStart = "/*"
    , commentEnd = "*/"
    , commentLine = "//"
    , reservedNames = ["newCalendar", "newEvent"
                      , "addEvent", "deleteEvent"
                      , "day", "week", "month"
                      , "allEvents"]
    , reservedOpNames = [":", "-"]
    }
  )

-- Parsea los comandos
parseCom :: Parser (CalCom)
parseCom = try parseNCalendar
       <|> try parseNEvent
       <|> try parseAEvent
       <|> try parseDEvent
       <|> try parseDay
       <|> try parseWeek
       <|> try parseMonth
       <|> try parseAllEvents

parseCal :: String -> Either ParseError CalCom
parseCal s = parse parseCom "" s

-- Parsea la operación para crear un nuevo calendario
parseNCalendar :: Parser CalCom
parseNCalendar = do reserved lisComm "newCalendar"
                    name <- identifier lisComm
                    return (NewCalendar name)

-- Parsea la operación para crear un nuevo evento
parseNEvent :: Parser CalCom
parseNEvent = do reserved lisComm "newEvent"
                 summary <- identifier lisComm
                 sDate <- parseDates
                 eDate <- parseDates
                 category <- optionMaybe (identifier lisComm)
                 return (NewEvent summary sDate eDate category)

-- Parsea la operación para agregar un evento
parseAEvent :: Parser CalCom
parseAEvent = do reserved lisComm "addEvent"
                 event <- parseEvent
                 return (AddEvent event)

-- Parsea la operación para eliminar un evento
parseDEvent :: Parser CalCom
parseDEvent = do reserved lisComm "deleteEvent"
                 event <- parseEvent
                 return (DeleteEvent event)

-- Parsea la operación para ver un día completo
parseDay :: Parser CalCom
parseDay = do reserved lisComm "day"
              return ThisDay

-- Parsea la operación para ver una semana completa
parseWeek :: Parser CalCom
parseWeek = do reserved lisComm "week"
               return ThisWeek

-- Parsea la operación para ver un mes completo
parseMonth :: Parser CalCom
parseMonth = do reserved lisComm "month"
                return ThisMonth

-- Parse la operación para ver todos los meses
parseAllEvents :: Parser CalCom
parseAllEvents = do reserved lisComm "allEvents"
                    return AllEvents

------------------------------------
------------------------------------

-- Funcion para facilitar el testing del parser
totParser :: Parser a -> Parser a
totParser p = do 
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , reservedNames   = ["date", "event"]
    , reservedOpNames = [":", "-"]
    }
  )

-- Parsea una fecha
parseDates :: Parser DateTime
parseDates = do day <- integer lis
                reservedOp lis "-"
                month <- integer lis
                reservedOp lis "-"
                year <- integer lis
                h <- integer lis
                reservedOp lis ":"
                m <- integer lis
                return (DateTime (fromIntegral year) (fromIntegral month) (fromIntegral day) (fromIntegral h) (fromIntegral m) 0)

-- Parsea un evento
parseEvent :: Parser Event
parseEvent = try (do reserved lis "event"
                     s <- identifier lis
                     st <- parseDates
                     et <- parseDates
                     c <- identifier lis
                     return (ECat (EventWithCat s st et c)))
             <|> (do reserved lis "event"
                     s <- identifier lis
                     st <- parseDates
                     et <- parseDates
                     return (ENoCat (EventWithoutCat s st et)))

------------------------------------
-- Función de parseo
------------------------------------

-- parseComm :: SourceName -> String -> Either ParseError CalCom
-- parseComm = parse (totParser comm) 
