module Calendar where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import DateTime
import Data.Char


-- Exercise 6
data Event = Event {
    --Required parameters
    dtstamp :: Dtstamp,
    uid :: Uid,
    dstart :: Dtstart,
    dend :: Dtend,
    --Optional parameters, nothing if not given.
    description :: Maybe Description,
    summary :: Maybe Summary,
    location :: Maybe Location
}
    deriving (Eq, Ord, Show)

type Dtstamp = DateTime
type Uid = String
type Dtstart = DateTime
type Dtend = DateTime
type Description = String
type Summary = String
type Location = String

data Calendar = Calendar {
    --version technically isn't needed because it can only have one value
    version :: Version,
    prodid :: Prodid,
    events :: [Event]
}
    deriving (Eq, Ord, Show)

type Prodid = String
data Version = Version2
    deriving (Eq, Ord, Show)

-- Exercise 7
type Token = (String,String)

scanCalendar :: Parser Char [Token]
scanCalendar = listOf p (token "\n")
    where p = (,) <$> textP <* symbol ':' <*> textP
          textP = many $ satisfy (/= '\r')
parseCalendar = undefined
-- parseCalendar :: Parser Token Calendar
-- parseCalendar = pack limiter p delimiter
--     where limiter = symbol ("BEGIN","VCALENDAR")
--           delimiter = symbol ("END","VCALENDAR")
--           versionP = Version2 <$ symbol ("VERSION","2.0")
--           prodidP = snd <$> satisfy ((=="PRODID") . fst)
--           p = (Calendar <$> )
--           pField s = mkG (snd <$> satisfy ((==s) . fst))
-- pField s p = P.mkG (P.pToken s *> P.pSym '=' *>) 
parseEvent :: Parser Token Event
parseEvent = undefined
recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar (Calendar version prodid events) = 
    "BEGIN:VCALENDAR"
    ++ "VERSION:2.0" 
    ++ "PRODID:" ++ prodid 
    ++ concatMap printEvent events
    ++ "END:VCALENDAR"

printEvent :: Event -> String
printEvent (Event dtstamp uid dstart dend description summary location) =
    "BEGIN:EVENT"
    ++ "DTSTAMP:" ++ show dtstamp
    ++ "UID:" ++ uid
    ++ "DSTART:" ++ show dstart
    ++ "DEND:" ++ show dend
    ++ maybePrint "DESCRIPTION:" description
    ++ maybePrint "SUMMARY:" summary
    ++ maybePrint "LOCATION" location
    ++ "END:EVENT"

maybePrint :: Show a => String -> Maybe a -> String
maybePrint _ Nothing = []
maybePrint s (Just a) = s ++ show a