module Media.NextEpisode(nextEpisodes,nextEpisode, addToPlaylist) where

import Media.Args(filterPaths')
import Media.IO(getVideosInfo,parseName,allMedia)
import Media.Misc(bashEscape)
import Media.Types(VideoInfo(..))

import System.FilePath(takeBaseName,takeDirectory)
import System.Process (runCommand)

import Data.List(sort)

pipe :: String
pipe = "~/.mplayer/pipe"

type Series = String
type EpNum  = Int

nextEpisode :: FilePath -> IO (Maybe FilePath)
nextEpisode path = do
    (vinfos,ser,n) <- getVideosInfos path
    let res = filter (nextEpFilter ser n) vinfos
    return $ 
        case res of
        [x]   -> Just $ filename x
        _     -> Nothing

    where
    nextEpFilter :: Series -> EpNum ->  VideoInfo -> Bool
    nextEpFilter curSeries  current VideoInfo{number=candidate,series=s} = 
        candidate == current + 1  && curSeries == s



nextEpisodes :: FilePath -> IO [FilePath]
nextEpisodes path = do
    (vinfos,ser,n) <- getVideosInfos path
    let res = filter (nextEpsFilter ser n) vinfos
    return $ map filename . sort $ res

    where
    nextEpsFilter :: Series -> EpNum ->  VideoInfo -> Bool
    nextEpsFilter curSeries  current VideoInfo{number=candidate,series=s} = 
        candidate > current  && curSeries == s


getVideosInfos :: FilePath -> IO ([VideoInfo],Series, EpNum)
getVideosInfos path =  do
    let VideoInfo{series=ser, number =n} = parseName name
    vinfos <- getVideosInfo (filterPaths' [ser]) allMedia dir
    return (vinfos,ser, n)

    where
    name = takeBaseName path
    dir  = takeDirectory path


addToPlaylist :: [FilePath] -> IO ()
addToPlaylist []     = return ()
addToPlaylist (x:xs) = do 
    putStrLn $ "Adding to playlist: " ++ x
    let esc     =  "playlist_clear"
        esc2    = bashEscape ("loadfile \"" ++ "" ++ x ++ "\" append") 
        command s = "echo  " ++ s ++ " > " ++ pipe
        commands = map command [esc,esc2]
    mapM_ (\s -> putStrLn $ "Command: " ++ s) commands
    {-runCommand $ commands !! 0-}
    runCommand $ commands !! 1
    
    addToPlaylist xs 

