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


nextEpFilter :: Series -> EpNum ->  VideoInfo -> Bool
nextEpFilter curSeries  current VideoInfo{number=candidate,series=s} = 
    candidate == current + 1  && curSeries == s


getVideosInfos :: FilePath -> IO ([VideoInfo],Series, EpNum)
getVideosInfos path =  do
    let VideoInfo{series=ser, number =n} = parseName name
    vinfos <- getVideosInfo (filterPaths' [ser]) allMedia dir
    return (vinfos,ser, n)

    where
    name = takeBaseName path
    dir  = takeDirectory path


addToPlaylist :: Maybe FilePath -> IO ()
addToPlaylist Nothing     = return ()
addToPlaylist (Just next) = do 
    putStrLn $ "Adding to playlist: " ++ next
    let esc     = bashEscape ("loadfile \"" ++ "" ++ next ++ "\" append")
        command = "echo  " ++ esc ++ " > " ++ pipe
    putStrLn $ "Command: " ++ command
    runCommand command
    return ()

