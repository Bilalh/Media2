{-# LANGUAGE OverloadedStrings #-}

module Media.IO
(   selectVideosInfo,selectVideosInfo', videosInfo, latest, oldest, SeriesKind(..),
    VideoFilter, FileFilter,FileSelector,
    defaultPath, videos,
    parseName, getVideosInfo,allMedia,
    getVideoData, getVideosInfo', media
) where

import Media.History
import Media.Types

import Control.Monad (forM_, forM)

import Data.Char
import Data.List(isSuffixOf)
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M

import System.Directory (doesDirectoryExist, getDirectoryContents, getHomeDirectory)
import System.Exit(exitFailure)
import System.FilePath ((</>), takeExtension, dropExtension, takeFileName)
import System.IO (stdout, hFlush)


import System.Console.ANSI
import Text.Printf

import qualified Data.Text as T

type VideoFilter  = (M.Map String [VideoInfo] -> M.Map String VideoInfo)
type FileFilter   = (M.Map String Int -> [VideoInfo] -> [VideoInfo])
type FileSelector = ([FilePath] -> [FilePath])
type VideoSupplier =(FilePath -> IO [FilePath])

getVideosInfo :: FileSelector -> FileFilter -> FilePath ->  IO [VideoInfo]
getVideosInfo = getVideosInfo' videos

getVideosInfo' ::VideoSupplier ->  FileSelector -> FileFilter -> FilePath ->  IO [VideoInfo]
getVideosInfo' vsp fsel ffunc path = do
    paths <- vsp path >>= return . fsel
    _infos <- videosInfo paths
    (infos,currents) <- findUnwatched ffunc _infos
    let classify' = classify currents

    let nums' = M.mapWithKey classify' infos
        res =  M.elems  $ infos
    return $ concat res

selectVideosInfo :: FileFilter -> FilePath -> VideoFilter ->  IO VideoInfo
selectVideosInfo = selectVideosInfo' id

selectVideosInfo' :: FileSelector -> FileFilter -> FilePath -> VideoFilter ->  IO VideoInfo
selectVideosInfo' fsel ffunc path func = do
    paths <- videos path >>= return . fsel
    _infos <- videosInfo paths
    (infos,currents) <- findUnwatched ffunc _infos
    let classify' = classify currents

    let nums' = M.mapWithKey classify' infos
        temp =  M.elems . func $  infos
    let res = zip temp [0..]

    case null res of
        True -> do
            putStr "No matching Files\n"
            exitFailure
        False -> do

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
            printf "%6s %s\n" ("" :: String) (series info)
            return ()
        index <- (pickEpPrompt . length) res
        return . fst $ res !! index

-- getVideoData id allMedia "/Users/bilalh/Movies/.Movies/Anime" latest
getVideoData :: FileSelector -> FileFilter -> FilePath -> VideoFilter ->IO [VideoData]
getVideoData  fsel ffunc path func = do
    paths <- videos path >>= return . fsel
    _infos <- videosInfo paths
    (infos,currents) <- findUnwatched ffunc _infos
    let classify' = classify currents

    let nums' = M.mapWithKey classify' infos
        temp =  M.elems . func $  infos
    let res = zip temp [0..]

    case null res of
        True -> do
            putStr "No matching Files\n"
            exitFailure
        False -> do

        let fin = (flip map) res $ \(info,n) -> do
            let c = fromMaybe 0 (M.lookup  (series info) currents)
            let (colour,str)  = numsLookUp info nums'
            VideoData{vNumber=n, vPrevious=c, vNext=str,  vSeries=(series info),vFilePath=(filename info) }
        return $ fin

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

latest :: VideoFilter
latest = M.map maximum

oldest :: VideoFilter
oldest = M.map minimum

allMedia :: FileFilter
allMedia  _ arr = arr

pickEpPrompt :: Int -> IO Int
pickEpPrompt upper = do
    printf "Choose an Episode to watch in [0,%d]\n> " $ upper-1
    hFlush stdout
    x <- getLine
    case filter (\(_,s) -> s == "") (reads x :: [(Int, String)]) of
      (x_int, _) : _ -> if inRange x_int 0 upper then  return x_int else pickEpPrompt upper
      _ -> pickEpPrompt upper


videosInfo :: [FilePath] -> IO (M.Map String [VideoInfo])
videosInfo paths =
   return $ toVideoMap (map parseName paths) M.empty

videos :: FilePath -> IO [FilePath]
videos =  ffind  vaildVideoExts

vaildVideoExts :: String -> Bool
vaildVideoExts name = not  (isSuffixOf "OCC" $ dropExtension name)
                     && any func [".mkv", ".mp4", ".avi",".webm", ".flv"]
                     where func allowed = allowed ==  map toLower (takeExtension name)

media = ffind exts
    where
    exts name = any func [ ".mkv"    , ".mp4"   , ".avi"  , ".webm" , ".flv"
                         , ".ogm"    , ".rm"    , ".rmvb" , ".divx" , ".wmv" , ".mpg"  , ".mov"
                         , ".aac"    , ".m4a"   , ".m4b"  , ".mp3"  , ".ogg" , ".flac"
                         , ".srt"    , ".ssa"   , ".ass"
                         , ".xdelta" , ".patch" , ".txt"  , ".md"]
         where func allowed = allowed ==  map toLower (takeExtension name)

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
        seriesName = func revStr

    in VideoInfo (rmOpEd seriesName) num filename

    where fix' =  dropWhile isPunctuation . dropWhile isSpace
          fix  =  dropWhile isSpace . fix'
          func =  map colonToSlash . dropWhile isSpace . reverse . fix



          -- To be consistent with the GUI
          colonToSlash ':' = '/'
          colonToSlash  a  = a

rmOpEd = rmEd .  rmOp

    where
    rmOp f  =
        case (T.breakOn " op " (T.pack f)) of
            (s, "") -> f
            (s, _)  -> T.unpack s

    rmEd f  =
        case (T.breakOn " ed " (T.pack f)) of
            (s, "") -> f
            (s, _)  -> T.unpack s

-- returns the name based on a function
ffind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
ffind p path = do
  names <- getRecursiveContents path
  return (filter p names)

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", "..", "pv", "oped" ]) names
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

defaultPath= do
    home <- getHomeDirectory
    let movies = home </> "Movies"
    b <-  doesDirectoryExist movies
    let res = if b then movies else home
    return res

