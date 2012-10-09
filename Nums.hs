import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC
import qualified Data.Map as M
import Text.Printf
import System.Environment(getArgs)
import Media.Misc

db    = "/Users/bilalh/Library/Application Support/Media/Media.db"

query = "SELECT Title, Current, Total, Date, Finished, Rewatching, Dropped \
    \FROM SeriesData \
    \WHERE( \
	\    strftime('%s',Date) > strftime('%s', 'now', ? ,'localtime') \
	\AND Finished = 0 AND (Skip = 0 AND Dropped = 0) \
    \)"
    
main = do
    args <- getArgs
    let days =  parseArgs args
    nums days

parseArgs :: [String] -> String
parseArgs [x] =  show (- parseIntCrash x) ++  " days"
parseArgs _  = "-7 days"
    
nums days= do
    conn <- connectSqlite3 db
    stmt <- prepare conn query
    a <- execute stmt [toSql days]
    rows <- fetchAllRows stmt
    let rows' = map (map (\b -> fromSql' b))  rows
    printRows rows'

printRows :: [[String]] -> IO ()
printRows []   = putStrLn "Nothing"
printRows z@[title:current:total:date:finished:rewatching:dropped:[]]  =  do 
    -- putStrLn $ show z
    -- putStrLn $ show title
    -- putStrLn $ show current
    -- putStrLn $ show total
    -- putStrLn $ show date
    -- putStrLn $ show finished
    -- putStrLn $ show rewatching
    -- putStrLn $ show dropped
    status <- case ( finished,  rewatching,  dropped) of 
        (_, "1", _) ->  return "R"
        ("1", _, _) ->  return "F"
        (_, _, "1") ->  return "D"
        (_, _, _) ->  return "O"
        
    -- TODO calcuate length
    -- TODO Change time to BST
    printf "%s %-*s %3s/%-3s %-28s\n"  status (41 :: Int) ( title) ( current) (total) ( date)

printRows (x:xs) = do
    printRows [x]
    printRows xs

fromSql'  SqlNull = "?"
fromSql'  arg = fromSql arg :: String