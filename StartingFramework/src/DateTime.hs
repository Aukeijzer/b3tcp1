module DateTime where

import ParseLib.Abstract
import qualified Data.Time as Time
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord)

instance Show DateTime where
    show = printDateTime

data Date = Date { year  :: Year
                , month :: Month
                , day   :: Day }
    deriving (Show, Eq, Ord)

newtype Year  = Year  { runYear  :: Int } deriving (Show, Eq, Ord)
newtype Month = Month { runMonth :: Int } deriving (Show, Eq, Ord)
newtype Day   = Day   { runDay   :: Int } deriving (Show, Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Show, Eq, Ord)

newtype Hour   = Hour   { runHour   :: Int } deriving (Show, Eq, Ord)
newtype Minute = Minute { runMinute :: Int } deriving (Show, Eq, Ord)
newtype Second = Second { runSecond :: Int } deriving (Show, Eq, Ord)


-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* symbol 'T' <*> parseTime <*> option (True <$ symbol 'Z') False

parseDate :: Parser Char Date
parseDate = Date <$> (Year <$> parseLong) <*>   (Month <$> parseShort) <*> (Day <$> parseShort)

parseTime :: Parser Char Time
parseTime = Time <$> (Hour <$> parseShort) <*> (Minute <$> parseShort) <*> (Second <$> parseShort)

parseLength :: Int -> Parser a a -> Parser a [a]
parseLength 0 p = succeed []
parseLength i p = (:) <$> p <*> parseLength (i-1) p
parseLong,parseShort :: Parser Char Int
parseLong = read <$> parseLength 4 digit
parseShort = read <$> parseLength 2 digit

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p s = let allOrNothing = const <$> p <*> eof
          in case parse allOrNothing s of
            [] -> Nothing
            (x:xs) -> Just $ fst x

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime date time utc) = printDate date ++ "T" ++ printTime time ++ printUTC utc

printDate :: Date -> String
printDate (Date y m d) = printLong (runYear y) ++ printShort (runMonth m) ++ printShort (runDay d)

printTime :: Time -> String
printTime (Time h m s) = printShort (runHour h) ++ printShort (runMinute m) ++ printShort (runSecond s)

printUTC :: Bool -> String
printUTC False = ""
printUTC True = "Z"

printLength :: Int -> Int -> String
printLength len i = zeroPart ++ intPart
    where intPart = show i
          zeroPart = replicate (len - length intPart) '0'
printLong,printShort :: Int -> String
printLong = printLength 4
printShort = printLength 2

-- Exercise 4
parsePrint :: [Char] -> Maybe String
parsePrint s = printDateTime <$> run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime date time _) = checkDate date && checkTime time

date2day :: Date -> Maybe Time.Day
date2day (Date y m d) = Time.fromGregorianValid (toInteger $ runYear y) (runMonth m) (runDay d)

checkDate :: Date -> Bool
checkDate date = case date2day date of
  Nothing -> False
  Just _ -> True

inRange :: Ord a => (a,a) -> a -> Bool
inRange (low,high) i = low <= i && i <= high

checkTime :: Time -> Bool
checkTime (Time h m s) = inRange (0, 23) (runHour h) && inRange (0,59) (runMinute m) && inRange (0,59) (runSecond s)