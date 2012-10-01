-- Categorise videos by series and presents a menu for playing them with mplayer

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeExtension, dropExtension, takeFileName)
import System.IO (stdout, hFlush)
import System.Process (runCommand)

import qualified Data.ByteString.Char8 as B
import qualified Text.ShellEscape as Esc
-- cabel install shell-escape

import Data.Char
import qualified Data.Map as M
import qualified Data.List as L

import System.Console.ANSI
import Text.Printf

path="/Users/bilalh/Movies/.Movies/Anime/"

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

-- returns name based on a function
ffind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
ffind p path = do
  names <- getRecursiveContents path
  return (filter p names)


vaildVideoExts :: [Char] -> Bool 
vaildVideoExts name = any (\allowed -> allowed == takeExtension name) [".mkv", ".mp4", ".avi",".webm"] 

videos :: FilePath -> IO [FilePath]
videos path =  ffind  vaildVideoExts path

s1 = "Fate:Zero 2 02.mkv"
s2 = "Fate:Zero.mkv"
s3 = "Fate:Zero    -   2.mkv"

data VideoInfo = VideoInfo { series   :: String 
                           , number   :: Int
                           , filename :: FilePath
                           } deriving (Show,Eq,Ord) 
 

-- Splits the filename into  (name, number, filepath)
parseName :: FilePath ->  VideoInfo
parseName filename =  let s = reverse $ dropExtension $ takeFileName filename
                      in case (span (isDigit) s) of 
                        ("", revStr) -> VideoInfo ( reverse  (fix revStr))  1 filename
                        (num,revStr) -> VideoInfo (reverse (fix revStr)) (read (reverse num)) filename
                      where fix' =  dropWhile isPunctuation  . dropWhile isSpace
                            fix  =  dropWhile isSpace . fix'
 
--  Create a map categorised by series
toVideoMap  :: [VideoInfo] -> M.Map String [VideoInfo] -> M.Map String [VideoInfo]
toVideoMap [] m      = m
toVideoMap (x:xs) m  = toVideoMap xs $ case M.lookup (series x) m of 
                         Nothing -> M.insert (series x) [x] m  
                         Just ls  -> M.insert (series x) (L.insert x ls) m

videosInfo :: FilePath -> IO (M.Map String [VideoInfo])              
videosInfo path = do
 names <- videos path
 return $ toVideoMap (map parseName names) M.empty
 

latest :: M.Map String [VideoInfo] -> M.Map String VideoInfo
latest m = M.map maximum m

type VideoFilter = (M.Map String [VideoInfo] -> M.Map String VideoInfo)
selectVideosInfo :: FilePath -> VideoFilter ->  IO VideoInfo
selectVideosInfo path func = do 
    infos <- videosInfo path
    let res = zip (M.elems $ func $ infos) [0..]
    forM (res) $ \(info,n) -> do 
        setSGR [ SetColor Foreground Vivid Green]
        printf "%-2d"  (n :: Int)  
        setSGR [ SetColor Foreground Vivid White]
        putStr " : P: "
        setSGR [ SetColor Foreground Dull White]
        printf "%-2d "  (number info)
        setSGR [ SetColor Foreground Vivid White]
        putStr "N: "
        setSGR [ SetColor Foreground Dull Yellow]
        printf "%2d " (number info)
        setSGR [ Reset]
        printf "%6s %s\n" "" (series info)
        return ()
    index <- pickEpPrompt $ length res
    return . fst $ res !! index 

pickEpPrompt :: Int -> IO Int
pickEpPrompt upper = do 
    printf "Choose an Episode to watch in [0,%d]\n" $ upper-1
    hFlush stdout
    x <- getLine
    case filter (\(_,s) -> s == "") (reads x :: [(Int, String)]) of
      (x_int, _) : _ -> if inRange x_int 0 upper then  return x_int else pickEpPrompt upper
      otherwise -> pickEpPrompt upper



inRange :: Int ->  Int -> Int -> Bool
inRange val lower upper 
    | val >= lower && val < upper  = True
    | otherwise                    = False

bashEscape :: String -> String
bashEscape str =  
    B.unpack . Esc.bytes . Esc.bash . B.pack $ str

--  command to run on the file
videoCommand :: VideoInfo -> String
videoCommand info = 
    "mplayer -input file=/Users/bilalh/.mplayer/pipe -input conf=input_no_enter.conf -geometry 0:0 -xy 480 -really-quiet " ++   bashEscape  (filename info)  ++ " &> /dev/null"

main = do 
    selected <- selectVideosInfo path latest
    pid <- runCommand $ videoCommand selected
    return pid