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
parseComm :: Parser (CalCom)
parseComm = try parseNCalendar
        <|> try parseNEvent
        <|> try parseAEvent
        <|> try parseDEvent
        <|> try parseDay
        <|> try parseWeek
        <|> try parseMonth
        <|> try parseAllEvents

parseCal :: String -> Either ParseError CalCom
parseCal s = parse parseComm "" s

-- Parsea la operación para crear un nuevo calendario
parseNCalendar :: Parser Comm
parseNCalendar = do reserved lisComm "newCalendar"
                    name <- identifier lisComm
                    return (NewCalendar name)

-- Parsea la operación para crear un nuevo evento
parseNEvent :: Parser Comm
parseNEvent = do reserved lisComm "newEvent"
                 summary <- parseStr
                 sDate <- parseDate
                 eDate <- parseDate
                 category <- optionMaybe parseStr
                 return (NewEvent summary sDate eDate category)

-- Parsea la operación para agregar un evento
parseAEvent :: Parser Comm
parseAEvent = do reserved lisComm "addEvent"
                 event <- parseEvent
                 return (AddEvent event)

-- Parsea la operación para eliminar un evento
parseDEvent :: Parser Comm
parseDEvent = do reserved lisComm "deleteEvent"
                 event <- parseEvent
                 return (DeleteEvent event)

-- Parsea la operación para ver un día completo
parseDay :: Parser Comm
parseDay = do reserved lisComm "day"
              return ThisDay

-- Parsea la operación para ver una semana completa
parseWeek :: Parser Comm
parseWeek = do reserved lisComm "week"
               return ThisWeek

-- Parsea la operación para ver un mes completo
parseMonth :: Parser Comm
parseMonth = do reserved lisComm "month"
                return ThisMonth

-- Parse la operación para ver todos los meses
parseAllEvents :: Parser Comm
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
parseDate :: Parser DateTime
parseDate = do day <- integer lis
               reservedOp lis "-"
               month <- integer lis
               reservedOp lis "-"
               year <- integer lis
               h <- integer lis
               reservedOp lis ":"
               m <- integer lis
               return (DateTime year month day h m 0)

-- Parsea un evento
parseEvent :: Parser Event
parseEvent = try (do reserved lis "event"
                     s <- parseStr
                     st <- parseDate
                     et <- parseDate
                     c <- parseStr
                     return (ECat (EventWithCat s st et c)))
             <|> (do reserved lis "event"
                     s <- parseStr
                     st <- parseDate
                     et <- parseDate
                     return (ENoCat (EventWithoutCat t st et))

------------------------------------
-- Función de parseo
------------------------------------

parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
