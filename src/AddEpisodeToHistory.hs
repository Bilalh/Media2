module Main where

import Control.Monad(void)

import Data.Time.Clock(NominalDiffTime,getCurrentTime,addUTCTime)
import Data.Maybe(isJust,fromJust,mapMaybe)
import qualified Data.Map as M

import Data.List.Split(splitOn)

import Media.History(addToHistory,labelFile,hideExtension)
import Media.Types (VideoInfo(..))
import Media.Misc (parseInt, getTimeStamp)
import Media.IO (parseName,videosInfo,videos)

import System.Environment(getArgs)
import System.FilePath(takeBaseName)
import System.Process (runCommand)


aPath :: FilePath
aPath = "/Users/bilalh/Movies/.Movies/Anime/"


type Series  = String
type EpNum   = Int
type Seconds = Int


main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run [s,t] | isJust m =  void $ addEpisodeToHistory s (fromJust m) 
    where m = parseTime t

run _     = return ()

parseTime :: String -> Maybe Seconds
parseTime s | length arr' == 3 = 
    Just $ foldl1  (\a b -> 60 * a +  b ) arr' 
    where arr  = splitOn ":" s
          arr' = mapMaybe parseInt arr

parseTime _ = Nothing


addEpisodeToHistory :: FilePath -> Seconds -> IO Integer
addEpisodeToHistory path elapsedSeconds= do
    let (ser,n) = parsePath path

    t      <- getCurrentTime
    let t' = addUTCTime (  toEnum (- elapsedSeconds * 1000000000000) :: NominalDiffTime ) t 

    res    <- addToHistory ser n  (getTimeStamp  t')

    processExtra res ser n
    return res

parsePath :: FilePath -> (Series,EpNum)
parsePath path = 
    let VideoInfo{series=ser, number =n} = parseName name
    in  (ser, n)

    where
    name = takeBaseName path


processExtra :: Integer -> Series -> EpNum ->  IO ()
processExtra  1 series' num = do
    videos' <- videos aPath >>= videosInfo 
    case M.lookup series' videos' of
        Nothing  ->  return ()
        Just arr ->  do
            let selected = filter (\(VideoInfo{number=n}) -> n == num) arr
            case selected of
                [x] -> do
                    runCommand $ labelFile (head selected) "orange"
                    runCommand $ hideExtension (head selected)
                    putStrLn $ "Labeled and hid Extension of " ++ show x
                    return ()
                _   -> return ()

processExtra _ _ _ =
    return ()

