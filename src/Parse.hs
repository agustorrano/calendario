module Parse where

import Text.Parsec
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language -- ( emptyDef )
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Char
import Data.Dates
import Data.Char
import Common

type P = Parsec String ()

------------------------------------

lexer :: Tok.TokenParser p
lexer = Tok.makeTokenParser langDef

langDef :: LanguageDef p
langDef = emptyDef 
    { commentStart = "/*"
    , commentEnd = "*/"
    , commentLine = "//"
    , reservedNames = ["newCalendar", "newEvent"
                      , "modify", "delete"
                      , "search", "category"
                      , "day", "week", "month"
                      , "allEvents", "date", "event"
                      , "every", "days", "weeks", "months"]
    , reservedOpNames = [":", "-"]
    }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer
natural = Tok.natural lexer

integer :: P Integer
integer = Tok.integer lexer

stringLiteral :: P String
stringLiteral = Tok.stringLiteral lexer

parens :: P a -> P a
parens = Tok.parens lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

------------------------------------
-- Parsers
------------------------------------

-- Parsea los comandos
parseCom :: P CalCom
parseCom = try parseNCalendar
       <|> try parseNEvent
       <|> try parseMEvent
       <|> try parseDEvent
       <|> try parseSEvent
       <|> try parseDay
       <|> try parseWeek
       <|> try parseMonth
       <|> try parseAllEvents
       <|> try parseCategory
       <|> try parseFreeTime

parseCal :: String -> Either ParseError CalCom
parseCal s = parse parseCom "" s

-- Parsea la operación para crear un nuevo calendario
parseNCalendar :: P CalCom
parseNCalendar = do reserved "newCalendar"
                    name <- identifier
                    return (NewCalendar name)

-- Parsea la operación para crear un nuevo evento
parseNEvent :: P CalCom
parseNEvent = do reserved "newEvent"
                 event <- parseEvent
                 return (NewEvent event)

-- Parsea la operación para agregar un evento
parseMEvent :: P CalCom
parseMEvent = do reserved "modify"
                 event <- parseEvent

                 return (ModifyEvent event)

-- Parsea la operación para eliminar un evento
parseDEvent :: P CalCom
parseDEvent = do reserved "delete"
                 event <- parseEvent
                 return (DeleteEvent event)

-- Parsea la operación para buscar un evento
parseSEvent :: P CalCom
parseSEvent = do reserved "search"
                 s <- identifier
                 return (SearchEvent s)

-- Parsea la operación para ver un día completo
parseDay :: P CalCom
parseDay = do reserved "day"
              return ThisDay

-- Parsea la operación para ver una semana completa
parseWeek :: P CalCom
parseWeek = do reserved "week"
               return ThisWeek

-- Parsea la operación para ver un mes completo
parseMonth :: P CalCom
parseMonth = do reserved "month"
                return ThisMonth

-- Parse la operación para ver todos los meses
parseAllEvents :: P CalCom
parseAllEvents = do reserved "allEvents"
                    return AllEvents

-- Parsea la operación que busca eventos de una misma categoría
parseCategory :: P CalCom
parseCategory = do reserved "category"
                   category <- identifier
                   return (Category category)

-- Parsea la operación que muestra el tiempo libre
parseFreeTime :: P CalCom
parseFreeTime = do reserved "freeTime"
                   return FreeTime

------------------------------------
------------------------------------

-- Funcion para facilitar el testing del parser
totParser :: P a -> P a
totParser p = do 
  whiteSpace
  t <- p
  eof
  return t

-- Parsea una fecha
parseDates :: P DateTime
parseDates = do day <- integer
                reservedOp "-"
                month <- integer
                reservedOp "-"
                year <- integer
                h <- integer
                reservedOp ":"
                m <- integer
                return (DateTime (fromIntegral year) (fromIntegral month) (fromIntegral day) (fromIntegral h) (fromIntegral m) 0)

-- Parsea la recurrencia diaria
parseDaily :: P Recurrence
parseDaily = do reserved "every"
                n <- integer
                (try (do reserved "day"
                         return (Daily (fromIntegral n)))
                 <|> do reserved "days"
                        return (Daily (fromIntegral n)))

-- Parsea la recurrencia semanal
parseWeekly :: P Recurrence
parseWeekly = do reserved "every"
                 n <- integer
                 (try (do reserved "week"
                          return (Weekly (fromIntegral n)))
                  <|> do reserved "weeks"
                         return (Weekly (fromIntegral n)))

-- Parsea la recurrencia mensual
parseMonthly :: P Recurrence
parseMonthly = do reserved "every"
                  n <- integer
                  (try (do reserved "month"
                           return (Monthly (fromIntegral n)))
                   <|> do reserved "months"
                          return (Monthly (fromIntegral n)))

-- Parsea la recurrencia de un evento
parseRecurrence :: P Recurrence
parseRecurrence = try parseDaily
                  <|> try parseWeekly
                  <|> parseMonthly                                 

-- Parsea un evento
parseEvent :: P Event
parseEvent = do reserved "event"
                s <- identifier
                st <- parseDates
                et <- parseDates
                c <- optionMaybe identifier
                r <- optionMaybe parseRecurrence
                return (Event s st et c r)

------------------------------------
-- Función de parseo
------------------------------------

parseThis :: String -> Either ParseError CalCom
parseThis s = parse parseCom "" s
