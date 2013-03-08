module Media.NextEpisode(nextEpisode,addToPlaylist) where

import Media.Args(filterPaths')
import Media.History(allMedia)
import Media.IO(getVideosInfo,parseName)
import Media.Misc(bashEscape)
import Media.Types(VideoInfo(..))

import System.FilePath(takeBaseName,takeDirectory)
import System.Process (runCommand)

pipe :: String
pipe = "~/.mplayer/pipe"

nextEpisode :: FilePath -> IO (Maybe FilePath)
nextEpisode path = do
    (vinfos,n) <- getVideosInfos path
    let res = filter (nextEpFilter n) vinfos
    return $ 
        case res of
        [x]    -> Just $ filename x
        (x:_) -> Just $ filename x
        _   -> Nothing


nextEpFilter :: Int -> VideoInfo -> Bool
nextEpFilter current VideoInfo{number=candidate} = candidate == current + 1 


getVideosInfos :: FilePath -> IO ([VideoInfo],Int)
getVideosInfos path =  do
    let VideoInfo{series=ser, number =n} = parseName name
    vinfos <- getVideosInfo (filterPaths' [ser]) allMedia dir
    return (vinfos, n)

    where
    name = takeBaseName path
    dir  = takeDirectory path


addToPlaylist :: Maybe FilePath -> IO ()
addToPlaylist Nothing     = return ()
addToPlaylist (Just next) = do 
    putStrLn $ "Adding to playlist: " ++ next
    let esc     = bashEscape ("loadfile \"" ++ "" ++ next ++ "\"")
        command = "echo  " ++ esc ++ " > " ++ pipe
    putStrLn $ "Command: " ++ command
    runCommand command
    return ()

