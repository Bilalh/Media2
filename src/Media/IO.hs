module Media.IO
( selectVideosInfo, selectVideosInfo', videosInfo, latest, oldest, SeriesKind(..)
) where

import Media.Types
import Media.History

import Data.List(isSuffixOf)
import Control.Monad (forM_, forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeExtension, dropExtension, takeFileName)
import System.IO (stdout, hFlush)

import Data.Maybe
import Data.Char
import qualified Data.Map as M
import qualified Data.List as L

import System.Console.ANSI
import Text.Printf

path="/Users/bilalh/Movies/.Movies/Anime/"

selectVideosInfo :: FilePath -> VideoFilter ->  IO VideoInfo
selectVideosInfo = selectVideosInfo' unwatched

type VideoFilter = (M.Map String [VideoInfo] -> M.Map String VideoInfo)
type FileFilter  = (M.Map String Int -> [VideoInfo] -> [VideoInfo])

selectVideosInfo' :: FileFilter -> FilePath -> VideoFilter ->  IO VideoInfo
selectVideosInfo' ffunc path func = do
    _infos <- videosInfo path
    (infos,currents) <- findUnwatched ffunc _infos
    let classify' = classify currents

    let nums' = M.mapWithKey classify' infos
    let res = zip ( M.elems . func $  infos) [0..]
    forM_ res $ \(info,n) -> do
        setSGR [ SetColor Foreground Vivid Green]
        printf "%-2d"  (n :: Int)
        setSGR [ SetColor Foreground Vivid White]
        putStr " : P: "
        setSGR [ SetColor Foreground Dull White]
        let c = fromMaybe 0 (M.lookup  (series info) currents)
        printf "%-2d " c
        setSGR [ SetColor Foreground Vivid White]
        putStr "N: "
        let (colour,str)  = numsLookUp info nums'
        setSGR [ SetColor Foreground Dull colour]
        printf "%-5s " str
        setSGR [ Reset]
        printf "%6s %s\n" "" (series info)
        return ()
    index <- (pickEpPrompt . length) res
    return . fst $ res !! index

numsLookUp :: VideoInfo -> M.Map String (t, SeriesKind) -> (Color, t)
numsLookUp info nums'=
    case M.lookup  (series info) nums' of
        Nothing   -> error "This should never happen"
        Just m    -> case m of
            (s,Sequential) -> (Blue,s)
            (s,Missing)    -> (Red,s)
            (s,Single)     -> (Yellow,s)
            (s,SingleGap)  -> (Magenta,s)


data SeriesKind = Sequential | Missing | Single | SingleGap

classify :: M.Map String Int -> String ->  [VideoInfo] -> (String,SeriesKind)
classify currents str arr =
    let current = fromMaybe 0 (M.lookup  str currents) in
    nums current arr

nums :: Int -> [VideoInfo] -> (String,SeriesKind)
nums cur [] = error "No infos given"
nums cur (VideoInfo{number=n}:[]) =
    if    n == 1 && cur == 1 
    ||     n == cur + 1      then (show n,Single)
    else                         (show n,SingleGap)

nums cur (VideoInfo{number=n}:xs) =
    let lastNum =  (number . last) xs
        sequential = length xs  == lastNum - n  in
    case sequential of
        True  -> (show  n ++ "-" ++ show lastNum,Sequential)
        False -> (show  n ++ "_" ++ show lastNum,Missing)

latest :: M.Map String [VideoInfo] -> M.Map String VideoInfo
latest = M.map maximum

oldest :: M.Map String [VideoInfo] -> M.Map String VideoInfo
oldest = M.map minimum

pickEpPrompt :: Int -> IO Int
pickEpPrompt upper = do
    printf "Choose an Episode to watch in [0,%d]\n> " $ upper-1
    hFlush stdout
    x <- getLine
    case filter (\(_,s) -> s == "") (reads x :: [(Int, String)]) of
      (x_int, _) : _ -> if inRange x_int 0 upper then  return x_int else pickEpPrompt upper
      _ -> pickEpPrompt upper


videosInfo :: FilePath -> IO (M.Map String [VideoInfo])
videosInfo path = do
   names <- videos path
   return $ toVideoMap (map parseName names) M.empty

videos :: FilePath -> IO [FilePath]
videos =  ffind  vaildVideoExts

vaildVideoExts :: String -> Bool
vaildVideoExts name = not  (isSuffixOf "OCC" $ dropExtension name)
                     && any func [".mkv", ".mp4", ".avi",".webm"]
                     where func allowed = allowed == takeExtension name

--  Create a map categorised by series
toVideoMap  :: [VideoInfo] -> M.Map String [VideoInfo] -> M.Map String [VideoInfo]
toVideoMap [] m      = m
toVideoMap (x:xs) m  = toVideoMap xs $ case M.lookup (series x) m of
                         Nothing -> M.insert (series x) [x] m
                         Just ls -> M.insert (series x) (L.insert x ls) m

-- Splits the filename into  (name, number, filepath)
parseName :: FilePath ->  VideoInfo
parseName filename =  
    let s = (reverse  . dropExtension . takeFileName) filename

        -- Remove episode name (if any)
        s' = case span (/= '|') s of
               (str, "") -> str
               (_,str)   -> (fix . tail)  str

        (revStr,num) =  case span isDigit s' of
                       ("", revStr) -> (revStr, 1)
                       (num,revStr) -> (revStr, (read . reverse) num)

    in VideoInfo (func revStr) num filename

    where fix' =  dropWhile isPunctuation . dropWhile isSpace
          fix  =  dropWhile isSpace . fix'
          func =  map colonToSlash . dropWhile isSpace . reverse . fix

          -- To be consistent with the GUI
          colonToSlash ':' = '/'
          colonToSlash  a  = a

-- returns the name based on a function
ffind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
ffind p path = do
  names <- getRecursiveContents path
  return (filter p names)

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

inRange :: Int ->  Int -> Int -> Bool
inRange val lower upper
  | val >= lower && val < upper  = True
  | otherwise                    = False
