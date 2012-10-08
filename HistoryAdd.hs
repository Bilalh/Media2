import System.Environment(getArgs)

import Data.Time.Format
import Data.List.Split 
import Data.Time.Clock

import Media.History
import Media.Types
import Media.Misc
import Media.Time
import Media.IO

import System.Process (runCommand)

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
        res <- addToHistory series lowerNum'  (getTimeStamp  t)
        processExtra res series lowerNum'
        return res
    where lowerNum' = parseIntError lowerNum

processArgs l@(series:lowerNum:date:[])  =  do
        t <- parseDate date
        res <- addToHistory series lowerNum'  (getTimeStamp  t)
        processExtra res series lowerNum'
        return res
    where lowerNum' = parseIntError lowerNum

processArgs _  = help

processExtra :: Integer -> String -> Int ->  IO ()
processExtra  1 series number = do
    putStrLn series
    videos <- videosInfo path
    case M.lookup series videos of
        Nothing  ->  return ()
        Just arr ->  do 
            let selected = filter (\(VideoInfo{number=n}) -> n == number) arr
            case selected of 
                [x] -> do
                    runCommand $ labelFile (selected !! 0) "orange"
                    runCommand $ hideExtension (selected !! 0)
                    putStrLn $ "Labeled and hid Extension of " ++ show x
                    return ()
                _   -> return ()
    
processExtra _ _ _ = do
    return ()    

-- colour here

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