import System.Environment(getArgs)

import Data.Time.Format
import Data.List.Split 
import Data.Time.Clock

import Media.History
import Media.Types
import Media.Misc
import Media.Time
import Media.IO

import qualified Data.Map as M

path="/Users/bilalh/Movies/.Movies/Anime/"

main = do
    args <- getArgs
    res <- processArgs  args
    putStrLn $ show res
    return ()

processArgs :: [String] -> IO Integer
processArgs l@(series:lowerNum:[])  =  do
        t <- getCurrentTime
        addToHistory series lowerNum'  (getTimeStamp  t)
    where lowerNum' = parseIntError lowerNum

processArgs l@(series:lowerNum:date:[])  =  do
        t <- parseDate date
        addToHistory series lowerNum'  (getTimeStamp  t)
    where lowerNum' = parseIntError lowerNum

processArgs _  = help



addTime :: UTCTime -> Time -> UTCTime
addTime utc time = let t =  timeToSeconds time
                       i =  fromInteger t :: Int
                       r = toEnum (i*1000000000000) :: NominalDiffTime in
    addUTCTime (r) utc

parseDate :: String -> IO UTCTime
parseDate timeStr=  do 
    now <- getCurrentTime
    case parseTimeString now timeStr of 
        Left _     -> return now 
        Right time -> return $ addTime now time
        
parseIntError :: String -> Int
parseIntError  num = case parseInt num of 
            Just d -> d
            Nothing -> help

help = error "Help"