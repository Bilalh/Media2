module Media.Time(
    Time(..), parseTimeString, timeToSeconds, toLocalTime,time', addTime
) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as PT
import Text.ParserCombinators.Parsec.Language (emptyDef)

import Data.Monoid
import Data.Time.Clock
import Data.Time
import Data.Time.Format
import qualified Data.Map as M

import System.Locale

import Media.Misc

data Time = Time {
     seconds :: Integer
    ,minutes :: Integer
    ,hours   :: Integer
    ,days     :: Integer
} deriving (Ord, Show, Eq)


timeAdd :: Time -> Time -> Time
timeAdd t1@(Time{seconds=s1, minutes=m1, hours=h1, days=d1})
        t2@(Time{seconds=s2, minutes=m2, hours=h2, days=d2}) =
    Time{seconds=s1+s2, minutes=m1+m2, hours=h1+h2, days=d1+d2}

instance Monoid Time where
    mempty = time'
    mappend = (timeAdd)

time' = Time{seconds=0,minutes=0,hours=0,days=0}

lexer :: PT.TokenParser st
lexer  = PT.makeTokenParser emptyDef

integer :: CharParser st Integer
integer = PT.integer lexer


inFunc :: String -> (Integer -> Time) ->  Parser Time
inFunc s func = do
    spaces
    string "in"
    i <- integer
    string s
    return $ func i

afterFunc :: String -> (Integer -> Time) ->  Parser Time
afterFunc s func = do
    spaces
    i <- integer
    string s
    spaces
    string "ago"
    return $ func (-i)

twrap s func = try (inFunc s func) <|> try (afterFunc s func)

inAfterTime ::  Parser [Time]
inAfterTime  = many (
           twrap "mins" (\i -> time'{minutes=i})
       <|> twrap "hours" (\i -> time'{hours=i})
       <|> twrap "days" (\i -> time'{hours=i})
     )

pm :: Date ->  Parser [Time]
pm startDate = ampm startDate "pm" 12
am :: Date ->  Parser [Time]
am startDate = ampm startDate "am" 0

namePart :: String  ->  Parser Time
namePart name = do
    string name
    return time'

minsPart :: String -> Parser Time
minsPart name = do
    string ":"
    mins <- integer

    string name
    return time'{minutes=mins}

ampm :: Date -> String -> Integer ->  Parser [Time]
ampm  startDate name fix = do
    h2' <- integer
    let h2 =  if h2' == 12 then h2' -12 else h2'

    mins  <-  try (minsPart name) <|> namePart name
    let diff h1 h2
          | h1 == h2  = 0
          | h2  > h2  = (h1 - h2)
          | otherwise = (h2 - h1)
    let hdiff = diff  ((toInteger $ h startDate))  (h2+fix)

    return $ mins:[ time'{hours = hdiff, minutes = -(toInteger $ m startDate), seconds = -(toInteger $ s startDate) }]

relDates :: Date ->  Parser [Time]
relDates date = do
    let t s' = try (string s')
    s <- t "sunday"  <|> t "monday" <|> t "tuesday" <|> t "wednesday"  <|> t "thursday"  <|> t "friday" <|> t "saturday"
    let iday = M.findWithDefault 0 s  daysMap
    let diff = dayDiff (snd $ day date) iday

    d <- try (pm date) <|> try (am date)
    return $  time'{days = toInteger diff} : d

-- d1 current  d2 old_day
dayDiff :: Int -> Int -> Int
dayDiff d1 d2
    | d1 == d2  = -7
    | d1 >  d2  = -(d1 - d2)
    | otherwise = -(d1 +7 - d2)


mainTime :: Date ->  Parser [Time]
mainTime startDate =  try(am startDate) <|> try(pm startDate) <|> try (relDates startDate)  <|>  try inAfterTime

timeString :: Date ->  Parser Time
timeString startDate = do
    arr <- mainTime startDate
    eof
    return $ mconcat arr

parseTimeString :: UTCTime -> String  -> Either ParseError Time
parseTimeString date input = parse (timeString $ parseDate date) "(unknown)" input

timeToSeconds :: Time -> Integer
timeToSeconds (Time{seconds=s1, minutes=m1, hours=h1, days=d1}) =
    s1  + m1 * 60 + h1 * 3600 + d1 * 86400

daysMap= M.fromList [
 	 ("sunday",0)
 	,("monday",1)
 	,("tueday",2)
 	,("wednesday",3)
 	,("thursday",4)
 	,("friday",5)
 	,("saturday",6)
 	]

data Date = Date {
     day   :: (String,Int)
    ,month :: (String,Int)
    ,year  :: Int
    ,h     :: Int
    ,m     :: Int
    ,s     :: Int
} deriving (Ord, Show, Eq)

parseDate :: UTCTime -> Date
parseDate t = let ft = formatTime defaultTimeLocale
                  d  = ft "%A" t
                  di = parseIntCrash $ ft "%w" t
                  m  = ft "%B" t
                  mi = parseIntCrash $ ft "%m" t
                  y  = parseIntCrash $ ft "%Y" t
                  h  = parseIntCrash $ ft "%k" t
                  mn = parseIntCrash $ ft "%M" t
                  s  = parseIntCrash $ ft "%S" t in

    Date{day=(d,di),month=(m,mi),year=y,h=h,m=mn, s =s}


addTime :: UTCTime -> Time -> UTCTime
addTime utc time = let t =  timeToSeconds time
                       i =  fromInteger t :: Int
                       r = toEnum (i*1000000000000) :: NominalDiffTime in
    addUTCTime r utc

-- Converts a utctime that really a localtime to a utctime
toLocalTime :: UTCTime -> IO UTCTime
toLocalTime t  = do
    tz <- getCurrentTimeZone
    let b = utcToLocalTime utc t
    return $ localTimeToUTC tz  b
