module Features where

import DateTime
import Calendar
import Text.PrettyPrint.Boxes
import qualified Data.Time as Time


-- Exercise 9
countEvents :: Calendar -> Int
countEvents = length . events

findEvents :: DateTime -> Calendar -> [Event]
findEvents time = filter (current time) . events

current :: DateTime -> Event -> Bool
current dt event = earlier dt (dstart event) && strictlyEarlier (dend event) dt

earlier :: DateTime -> DateTime -> Bool
earlier dt1 dt2 = diffDays dt1 dt2  < 0 || (diffDays dt1 dt2 == 0 && diffTime dt1 dt2 <=0)
strictlyEarlier :: DateTime -> DateTime -> Bool
strictlyEarlier dt1 dt2 = diffDays dt1 dt2 < 0 || (diffDays dt1 dt2 == 0 && diffTime dt1 dt2 < 0)

diffDays :: DateTime -> DateTime -> Integer
diffDays dt1 dt2 = Time.diffDays (toDay dt1) (toDay dt2)
    where toDay = (\(Just x) -> x) . date2day . date
diffTime :: DateTime -> DateTime -> Int
diffTime dt1 dt2 = diffSeconds (time dt1) (time dt2)
    where diffSeconds (Time h1 m1 s1) (Time h2 m2 s2) = ((runHour h1 - runHour h2) * 60) + (runMinute m1 - runMinute m2) * 60 + (runSecond s1 - runSecond s2)

--
checkOverlapping :: Calendar -> Bool
checkOverlapping calendar = any hasOverlapping calEvents
    where calEvents = events calendar
          hasOverlapping :: Event -> Bool
          hasOverlapping currentEvent = fst $ foldr check (True,False) calEvents
            where check :: Event -> (Bool,Bool) -> (Bool,Bool)
                  check _ (False,_) = (False,False)
                  check event (True,False) | event == currentEvent = (True,True)
                                           | otherwise = (overlapping event currentEvent,True)
                  check event (True,True) = (overlapping event currentEvent,True)
                  overlapping event1 event2 = earlier (dstart event1) (dend event2) || earlier (dstart event2) (dend event1)

timeSpent :: String -> Calendar -> Int
timeSpent s calendar = sum $ map duration validEvents
    where validEvents = filter ((==Just s) . summary) (events calendar)

duration :: Event -> Int
duration event = fromInteger (diffDays start end) * (24*60*60) + diffTime start end
    where start = dstart event
          end = dend event
-- Exercise 10
ppMonth :: Year -> Month -> Calendar -> String
ppMonth y m calendar = render box
    where validEvents = filter
          box = undefined

