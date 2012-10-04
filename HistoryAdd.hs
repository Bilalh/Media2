import System.Environment(getArgs)

import Data.Time.Format
import Data.List.Split 

import Data.Time.Clock


import Media.History
import Media.Types


func str = return []

main = do
    args <- getArgs
    return $ processArgs $ args

processArgs :: [String] -> IO Integer
processArgs l@(series:lowerNum:[])  =  do
        timeStamp <- getTimeStamp
        addToHistory series lowerNum'  timeStamp
    where lowerNum' = parseIntError lowerNum


processArgs _  = help

getTimeStamp :: IO [Char]
getTimeStamp = do 
    t <- getCurrentTime
    let res = span (/= '.') $ show t
    return $ fst res

parseIntError :: String -> Int
parseIntError  num = case parseInt num of 
            Just d -> d
            Nothing -> help 

parseInt :: String -> Maybe Int
parseInt s  =  case reads s :: [(Int,String)] of 
            [(d,"")] -> Just d
            otherwise -> Nothing

help = error "Help"

-- splitOn " "
-- parseDateTime :: [String] -> IO String
parseDateTime l@("in": num: "mins":[])  = do 
    t <- getCurrentTime
    return t
    
parseDateTime _ = error "not imp"


ten_seconds_before = addUTCTime (-10)

