module Media.History
(
    findUnwatched, unwatched,
    addToHistory, labelFile, hideExtension
) where

import Control.Monad (forM)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

import Media.Types
import Media.Misc
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC

-- CHANGE this to the location of your sqlite3 database
db    = "/Users/bilalh/Library/Application Support/Media/Media.db"
unwatchedQuery = "SELECT current from SeriesData WHERE Title = replace(ltrim(?),':','/')"

-- returns all the Episodes which have not been watched based on the supplied map
unwatched :: M.Map String Int -> [VideoInfo] -> [VideoInfo]
unwatched _ [] = []
unwatched currents (v@VideoInfo{number=n, series=s}:[]) =
    case M.lookup s  currents of
        Nothing   -> [v]
        Just m    -> if n > m then [v] else []
unwatched currents (x:xs) =  unwatched currents [x]  ++  unwatched currents xs

findUnwatched :: (M.Map String Int -> [VideoInfo] -> [VideoInfo]) 
              -> M.Map String [VideoInfo] 
              ->  IO (M.Map String [VideoInfo], M.Map String Int)
findUnwatched func infos = do
    conn <- connectSqlite3 db
    stmt <- prepare conn unwatchedQuery

    --  The most recent episode for each series
    currentL <- forM (M.keys infos) $ \name -> do
        execute stmt [toSql name]
        r <- fetchRow stmt
        case r of
            Nothing   -> return (name,0)
            Just str  -> return (name, read . fromSql $ head str  :: Int)
    let current  = M.fromList currentL

    let res = M.map (\a -> func current a) infos
    let res' = M.filterWithKey (\_ v -> v /= [] ) res
    disconnect conn
    return (res', current)

addQuery = "INSERT INTO History(Series,Number,Date) Values(?,?,?)"

addToHistory :: String -> Int -> String -> IO Integer
addToHistory series  num date = do
    conn <- connectSqlite3 db
    stmt <- prepare conn addQuery
    res <- execute stmt [toSql series, toSql num, toSql date]
    commit conn
    disconnect conn
    return res

labelFile :: VideoInfo ->  String -> String
labelFile v@VideoInfo{filename=f} colour = let ef = bashEscape f in
    "SetLabel "  ++ colour ++ " " ++ ef ++ " &>/dev/null"

hideExtension :: VideoInfo -> String
hideExtension v@VideoInfo{filename=f} = let ef = bashEscape f in
    "hide_extension.applescript "  ++  ef ++ " &>/dev/null"
