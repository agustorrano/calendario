module Parse where

import Text.Parsec
  ( eof
  , optionMaybe
  , sepBy
  , (<|>)
  , parse
  , try
  , many1
  , optional
  , space
  , ParseError
  , Parsec
  , manyTill
  , lookAhead )
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Char
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
  , reservedNames = ["Cal", "C", "E"
                    , "modify", "delete"
                    , "search", "category"
                    , "day", "week", "month"
                    , "all", "date", "every"
                    , "days", "weeks", "months"]
  , reservedOpNames = [":", ">", "/"]
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

brackets :: P a -> P a
brackets = Tok.brackets lexer

comma :: P String
comma = Tok.comma lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

------------------------------------
-- | Parsers
------------------------------------

num :: P Int
num = do
  n <- many1 digit
  return $ read n

-- | Parsea los comandos
-- |
parseCom :: P CalCom
parseCom = 
  try parseNCalendar
  <|> try parseNEvent
  <|> try parseMEvent
  <|> try parseDEvent
  <|> try parseSEvent
  <|> try parseDay
  <|> try parseWeek
  <|> try parseMonth
  <|> try parseAllEvents
  <|> try parseCategory

-- | Parsea la operación para crear un nuevo calendario
-- |
parseNCalendar :: P CalCom
parseNCalendar = do 
  reserved "C"
  NewCalendar <$> identifier

-- | Parsea la operación para crear un nuevo evento
-- |
parseNEvent :: P CalCom
parseNEvent = do 
  reserved "E"
  NewEvent <$> parseEvent

-- | Parsea la operación para agregar un evento
-- |
parseMEvent :: P CalCom
parseMEvent = do 
  reserved "modify"
  ModifyEvent <$> parseEventE

-- | Parsea la operación para eliminar un evento
-- |
parseDEvent :: P CalCom
parseDEvent = do 
  reserved "delete"
  DeleteEvent <$> parseEventE

-- | Parsea la operación para buscar un evento
-- |
parseSEvent :: P CalCom
parseSEvent = do 
  reserved "search"
  SearchEvent <$> identifier

-- | Parsea la operación para ver un día completo
-- |
parseDay :: P CalCom
parseDay = do 
  reserved "day"
  return ThisDay

-- | Parsea la operación para ver una semana completa
-- |
parseWeek :: P CalCom
parseWeek = do 
  reserved "week"
  return ThisWeek

-- | Parsea la operación para ver un mes completo
-- |
parseMonth :: P CalCom
parseMonth = do 
  reserved "month"
  return ThisMonth

-- | Parsea la operación para ver todos los meses
-- |
parseAllEvents :: P CalCom
parseAllEvents = do 
  reserved "all"
  return AllEvents

-- | Parsea la operación que busca eventos de una misma categoría
-- |
parseCategory :: P CalCom
parseCategory = do 
  reserved "category"
  Category <$> identifier

------------------------------------
------------------------------------

-- | Funcion para facilitar el testing del parser
-- |
totParser :: P a -> P a
totParser p = do
  whiteSpace
  t <- p
  eof
  return t

-- | Parsea una fecha con minutos
-- |
parseDates :: P (DateTime, Bool)
parseDates = try (do 
  day <- integer
  reservedOp "/"
  month <- integer
  reservedOp "/"
  year <- integer
  h <- num
  reservedOp ":"
  m <- integer
  return 
      (DateTime (fromIntegral day, fromIntegral month,
      fromIntegral year, fromIntegral h, fromIntegral m), False))

-- | Parsea una fecha sin minutos
-- |
parseDateNoMin :: P (DateTime, Bool)
parseDateNoMin = try (do 
  day <- integer
  reservedOp "/"
  month <- integer
  reservedOp "/"
  year <- integer
  h <- num
  optional space
  return 
      (DateTime (fromIntegral day, fromIntegral month,
      fromIntegral year, fromIntegral h, 0), False))

-- | Parsea una fecha sin hora
-- |
parseDateNoHour :: P (DateTime, Bool)
parseDateNoHour = do 
  day <- integer
  reservedOp "/"
  month <- integer
  reservedOp "/"
  year <- integer
  return 
      (DateTime (fromIntegral day, fromIntegral month,
      fromIntegral year, 0, 0), True)

-- | Parsea la recurrencia diaria
-- |
parseDaily :: P Recurrence
parseDaily = do 
  reserved "every"
  n <- integer
  try (do 
    reserved "day"
    return (Daily (fromIntegral n)))
   <|> do 
    reserved "days"
    return (Daily (fromIntegral n))

-- | Parsea la recurrencia semanal
-- |
parseWeekly :: P Recurrence
parseWeekly = do 
  reserved "every"
  n <- integer
  try (do 
    reserved "week"
    return (Weekly (fromIntegral n)))
   <|> do 
    reserved "weeks"
    return (Weekly (fromIntegral n))

-- | Parsea la recurrencia mensual
-- |
parseMonthly :: P Recurrence
parseMonthly = do 
  reserved "every"
  n <- integer
  try (do 
    reserved "month"
    return (Monthly (fromIntegral n)))
   <|> do 
    reserved "months"
    return (Monthly (fromIntegral n))

-- | Parsea la recurrencia de un evento
-- |
parseRecurrence :: P Recurrence
parseRecurrence = 
  try parseDaily
  <|> try parseWeekly
  <|> parseMonthly

-- | Parsea el título de un evento
-- |
parseSummary :: P String
parseSummary = manyTill anyChar (try (lookAhead (space >> parseDate)))

------------------------------------
-- | Parsers para eventos
------------------------------------

first :: DateTime -> Int
first (DateTime (d, _, _, _, _)) = d

second :: DateTime -> Int
second (DateTime (_, m, _, _, _)) = m

third :: DateTime -> Int
third (DateTime (_, _, y, _, _)) = y

-- | Parsea la fecha y hora de finalización
-- |
parseET1 :: P DateTime
parseET1 = do
  reservedOp "-"
  optional space
  (et, b') <- try parseDates <|> try parseDateNoMin
  return et

-- | Parsea la hora de finalización con minutos
-- |
parseET2 :: DateTime -> P DateTime
parseET2 st = do
  reservedOp "-"
  optional space
  h <- num
  reservedOp ":"
  m <- integer
  let d = first st
      mon = second st
      y = third st
      et = DateTime (d, mon, y, fromIntegral h, fromIntegral m)
  return et

-- | Parsea la hora de finalización sin minutos
-- |
parseET3 :: DateTime -> P DateTime
parseET3 st = do
  reservedOp "-"
  optional space
  h <- num
  optional space
  let d = first st
      mon = second st
      y = third st
      et = DateTime (d, mon, y, fromIntegral h, 0)
  return et

-- | Parsea la categoría y recurrencia si es que el evento lo tiene
-- |
parseCatRec :: P (Maybe Category, Maybe Recurrence)
parseCatRec = do
  c <- optionMaybe identifier
  r <- optionMaybe parseRecurrence
  return (c, r)

-- | Parsea un evento de día completo
-- |
parseEvTrue :: P (Maybe DateTime, Maybe Category, Maybe Recurrence)
parseEvTrue = 
  try (do 
    reservedOp "-"
    (et, b') <- parseDateNoHour
    (c, r) <- parseCatRec
    return (Just et, c, r))
  <|> do 
    (c, r) <- parseCatRec
    return (Nothing, c, r)

parseDate :: P (DateTime, Bool)
parseDate = parseDates <|> parseDateNoMin <|> parseDateNoHour

-- | Parsea un evento
-- |
parseEvent :: P Event
parseEvent = do
  s <- parseSummary
  (st, b) <- parseDate
  if b
  then do 
    (jet, c, r) <- parseEvTrue
    case jet of
      Nothing -> return (Event s st st c r b)
      Just et -> return (Event s st et c r b)
  else do
    et <- try parseET1 <|> try (parseET2 st) <|> parseET3 st
    (c, r) <- parseCatRec
    return (Event s st et c r b)
  

parseEventE :: P Event
parseEventE = do
  reserved "E"
  s <- parseSummary
  (st, b) <- parseDate
  if b
  then do 
    (jet, c, r) <- parseEvTrue
    case jet of
      Nothing -> return (Event s st st c r b)
      Just et -> return (Event s st et c r b)
  else do
    et <- try parseET1 <|> try (parseET2 st) <|> parseET3 st
    (c, r) <- parseCatRec
    return (Event s st et c r b)

-- | Parsea una lista de eventos
-- |
parseList :: P [Event]
parseList = sepBy parseEventE comma

parseCal :: P Calendar
parseCal = do 
  reserved "C"
  name <- identifier
  Calendar name <$> parseList

------------------------------------
-- | Función de parseo
------------------------------------

parseThis :: String -> Either ParseError CalCom
parseThis = parse parseCom ""
