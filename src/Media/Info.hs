module Media.Info (
    nums_main, queryLatest, 
    queryLatestWithFinished, 
    queryLatestWithFinishedAndSkipped,
    queryLatestWithSkipped
) where

import Data.Time.Clock
import Data.Time
import Data.Time.Format

import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC
import qualified Data.Map as M
import Text.Printf

import System.Environment(getArgs)
import System.Console.ANSI
import System.Locale

import Media.Misc
import Terminal


db    = "/Users/bilalh/Library/Application Support/Media/Media.db"

queryLatest = "SELECT Title, Current, Total, Date, Finished, Rewatching, Dropped \
    \FROM SeriesData \
    \WHERE( \
	\    strftime('%s',Date) > strftime('%s', 'now', ? ,'localtime') \
	\AND Finished = 0 AND (Skip = 0 AND Dropped = 0) \
    \)"

queryLatestWithFinished = "SELECT Title, Current, Total, Date, Finished, Rewatching, Dropped \
    \FROM SeriesData \
    \WHERE( \
	\    strftime('%s',Date) > strftime('%s', 'now', ? ,'localtime') \
	\AND (Dropped = 0) \
    \)"

queryLatestWithFinishedAndSkipped = "SELECT Title, Current, Total, Date, Finished, Rewatching, Dropped \
    \FROM SeriesData \
    \WHERE( \
	\    strftime('%s',Date) > strftime('%s', 'now', ? ,'localtime') \
    \)"

queryLatestWithSkipped = "SELECT Title, Current, Total, Date, Finished, Rewatching, Dropped \
    \FROM SeriesData \
    \WHERE( \
	\    strftime('%s',Date) > strftime('%s', 'now', ? ,'localtime') \
	\AND Finished = 0 AND (Finished = 0) \
    \)"

nums_main :: String -> IO ()
nums_main query = do
    args <- getArgs
    let days =  parseArgs args
    nums days query

parseArgs :: [String] -> String
parseArgs [x] =  show (- parseIntCrash x) ++  " days"
parseArgs _  = "-11 days"

nums days query= do
    conn <- connectSqlite3 db
    stmt <- prepare conn query
    a <- execute stmt [toSql days]
    rows <- fetchAllRows stmt
    let rows' = map (map fromSql' )  rows
    printRows rows'

printRows :: [[String]] -> IO ()
printRows []   = putStrLn "Nothing"
printRows z@[title:current:total:date:finished:rewatching:dropped:[]]  =  do
    (status,colour) <- case ( finished,  rewatching,  dropped) of
        (_, "1", _) ->  return ("R",Green)
        ("1", _, _) ->  return ("F", Cyan)
        (_, _, "1") ->  return ("D", White)
        (_, _, _) ->  return ("O", Yellow)

    setSGR [ SetColor Foreground Dull colour]
    putStr status
    setSGR [ Reset]

    let utcDate = readTime defaultTimeLocale "%F %H:%M:%S" date :: UTCTime
    tz <- getCurrentTimeZone
    let localDate = utcToLocalTime tz utcDate
    let date' = formatTime defaultTimeLocale "%Y-%m-%d %H:%M %a %d %b" localDate

    titleLength <- getTitleLength 39

    printf " %-*s " titleLength  title
    setSGR [ SetColor Foreground Dull Cyan]
    printf "%3s" current
    putStr "/"
    setSGR [ SetColor Foreground Dull Red]
    printf "%-3s " total
    setSGR [ Reset]
    printf "%-28s\n"   date'

printRows (x:xs) = do
    printRows [x]
    printRows xs

fromSql'  SqlNull = "?"
fromSql'  arg = fromSql arg :: String
