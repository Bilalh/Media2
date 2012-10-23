import System.Environment(getArgs)

import Data.Time.Format
import Data.List.Split
import Data.Time.Clock
import Data.Char

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
    return ()


processArgs :: [String] -> IO Integer
processArgs l@(series:lowerNum:[])  =  do
        t <- getCurrentTime
        res <- addToHistory series lowerNum'  (getTimeStamp  t)
        processExtra res series lowerNum'
        return res
    where lowerNum' = parseIntError lowerNum

processArgs l@(series:lowerNum:date:[])  =  do
        t' <- parseDate date
        t  <- toLocalTime t'
        res <- addToHistory series lowerNum'  (getTimeStamp  t)
        processExtra res series lowerNum'
        return res
    where lowerNum' = parseIntError lowerNum

processArgs _  = help

processExtra :: Integer -> String -> Int ->  IO ()
processExtra  1 series number = do
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

processExtra _ _ _ =
    return ()

parseDate :: String -> IO UTCTime
parseDate timeStr=  do
    now <- getCurrentTime
    case parseTimeString now (map toLower timeStr) of
        Left _     -> error $ "Could not parse " ++ timeStr
        Right time -> return $ addTime now time


parseIntError :: String -> Int
parseIntError  num = case parseInt num of
            Just d -> d
            Nothing -> help

help = error "histb Series lower_num [time]"
