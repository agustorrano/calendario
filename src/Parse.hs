module Parse where

import Text.Parsec
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language -- ( emptyDef )
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Char
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
    , reservedOpNames = [":", "-", "/"]
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

parseCal :: String -> Either ParseError CalCom
parseCal = parse parseCom ""

-- Parsea la operación para crear un nuevo calendario
parseNCalendar :: P CalCom
parseNCalendar = do reserved "newCalendar"
                    NewCalendar <$> identifier

-- Parsea la operación para crear un nuevo evento
parseNEvent :: P CalCom
parseNEvent = do reserved "newEvent"
                 NewEvent <$> parseEvent

-- Parsea la operación para agregar un evento
parseMEvent :: P CalCom
parseMEvent = do reserved "modify"
                 ModifyEvent <$> parseEvent

-- Parsea la operación para eliminar un evento
parseDEvent :: P CalCom
parseDEvent = do reserved "delete"
                 DeleteEvent <$> parseEvent

-- Parsea la operación para buscar un evento
parseSEvent :: P CalCom
parseSEvent = do reserved "search"
                 SearchEvent <$> identifier

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
                   Category <$> identifier

------------------------------------
------------------------------------

-- Funcion para facilitar el testing del parser
totParser :: P a -> P a
totParser p = do
  whiteSpace
  t <- p
  eof
  return t

-- Parsea una fecha con minutos
parseDates :: P (DateTime, Bool)
parseDates =
  try (do day <- integer
          reservedOp "/"
          month <- integer
          reservedOp "/"
          year <- integer
          h <- integer
          reservedOp ":"
          m <- integer
          return ((fromIntegral day,
                   fromIntegral month,
                   fromIntegral year,
                   fromIntegral h,
                   fromIntegral m), False))

-- Parsea una fecha sin minutos
parseDateNoMin :: P (DateTime, Bool)
parseDateNoMin =
  try (do day <- integer
          reservedOp "/"
          month <- integer
          reservedOp "/"
          year <- integer
          h <- integer
          return ((fromIntegral day,
                   fromIntegral month,
                   fromIntegral year,
                   fromIntegral h, 0), False))

-- Parsea una fecha sin hora
parseDateNoHour :: P (DateTime, Bool)
parseDateNoHour =
  try (do day <- integer
          reservedOp "/"
          month <- integer
          reservedOp "/"
          year <- integer
          return ((fromIntegral day,
                   fromIntegral month,
                   fromIntegral year, 0, 0), True))

-- Parsea la recurrencia diaria
parseDaily :: P Recurrence
parseDaily = do reserved "every"
                n <- integer
                try (do reserved "day"
                        return (Daily (fromIntegral n)))
                 <|> do reserved "days"
                        return (Daily (fromIntegral n))

-- Parsea la recurrencia semanal
parseWeekly :: P Recurrence
parseWeekly = do reserved "every"
                 n <- integer
                 try (do reserved "week"
                         return (Weekly (fromIntegral n)))
                  <|> do reserved "weeks"
                         return (Weekly (fromIntegral n))

-- Parsea la recurrencia mensual
parseMonthly :: P Recurrence
parseMonthly = do reserved "every"
                  n <- integer
                  try (do reserved "month"
                          return (Monthly (fromIntegral n)))
                   <|> do reserved "months"
                          return (Monthly (fromIntegral n))

-- Parsea la recurrencia de un evento
parseRecurrence :: P Recurrence
parseRecurrence = try parseDaily
                  <|> try parseWeekly
                  <|> parseMonthly

first :: DateTime -> Int
first (d, _, _, _, _) = d

second :: DateTime -> Int
second (_, m, _, _, _) = m

third :: DateTime -> Int
third (_, _, y, _, _) = y

-- Parsea un evento
parseEvent :: P Event
parseEvent =
  do reserved "event"
     s <- identifier
     (st, b) <- parseDates <|> parseDateNoMin <|> parseDateNoHour
     if b
     then do reservedOp "-"
             (et, b') <- parseDateNoHour
             c <- optionMaybe identifier
             r <- optionMaybe parseRecurrence
             return (Event s st et c r b)
           <|> do c <- optionMaybe identifier
                  r <- optionMaybe parseRecurrence
                  return (Event s st st c r b)
     else try (do reservedOp "-"
                  (et, b') <- parseDates <|> parseDateNoMin
                  c <- optionMaybe identifier
                  r <- optionMaybe parseRecurrence
                  return (Event s st et c r b)
               <|> try (do reservedOp "-"
                           h <- integer
                           reservedOp ":"
                           m <- integer
                           let d = first st
                               mon = second st
                               y = third st
                               et = (d, mon, y, fromIntegral h, fromIntegral m)
                           c <- optionMaybe identifier
                           r <- optionMaybe parseRecurrence
                           return (Event s st et c r b))
               <|> do reservedOp "-"
                      h <- integer
                      let d = first st
                          mon = second st
                          y = third st
                          et = (d, mon, y, fromIntegral h, 0)
                      c <- optionMaybe identifier
                      r <- optionMaybe parseRecurrence
                      return (Event s st et c r b))

------------------------------------
-- Función de parseo
------------------------------------

parseThis :: String -> Either ParseError CalCom
parseThis = parse parseCom ""
