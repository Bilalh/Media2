{-# LANGUAGE DeriveDataTypeable #-}

import Data.Time.Clock
import Media.IO
import Media.Player
import Media.Types
import Media.Misc
import Media.History
import System.Process (runCommand)
import System.Environment (getArgs)
import System.Directory (getHomeDirectory,doesDirectoryExist)
import System.FilePath ( (</>))

import qualified Data.Map as M

import System.Console.CmdArgs

defaultPath= do 
    home <- getHomeDirectory
    let movies = home </> "Movies"
    b <-  doesDirectoryExist movies
    let res = (if b then movies else home)
    return res


data VFilter = Oldest    | Latest deriving (Data, Typeable, Show)
data FFilter = Unwatched | All    deriving (Data, Typeable, Show)

data Media2 = Media2 {
    vFilter :: VFilter, 
    fFilter :: FFilter, 
    vPlayer :: PlayerType,
    history :: Bool,
    path    :: FilePath
    }
    deriving (Data, Typeable, Show)

-- Categorise videos by series and presents a menu for playing them with a player
main = do
    args <- getArgs
    opts <- cmdArgs $ getOpts
    opts' <- fillInOpts opts 
    _ <- play opts'
    -- return ()
    return opts'

fillInOpts :: Media2 -> IO Media2
fillInOpts opts@(Media2{path=p} ) | p == ""  =do 
    def <- defaultPath
    return opts{path=def}

fillInOpts m = return m

-- Parse the command line options
getOpts :: Media2
getOpts =
    Media2{
        vFilter = enum
            [ Oldest &= help "Prefer Older files (default)"
            , Latest &= help "Only play the newest file"
            ],
        fFilter = enum
            [ Unwatched &= help "Only plays unwatched files (default)"
            , All       &= help "Plays all files"
            ],
        vPlayer = enum
            [ MPlayerOSX &= name "m"  &= help "Use MPlayerOSX as the player (Deafult)"
            , MPlayerX   &= name "x"  &= help "Use MPlayerX as the player"
            , MPlayer    &= name "M"  &= help "Use MPlayer (command line) as the player"
            , MPV        &= name "v"  &= help "Use mpv (command line) as the player"
            , MPV_App    &= name "V"  &= help "Use mpv (app) as the player"
            , VLC                     &= help "Use VLC as the player"
            ],
        history = def &= name "y" &= help "Add files to history",
        path    = def &= name "p" &= help "Directory to look for files inculdes sub dirs " &= typDir
        } &=
        versionArg [ignore] &=
        program "media2" &=
        help "Categorise videos by series and presents a menu for playing them." &=
        summary "Media' v2.0 (C) Bilal Syed Hussain" 



vfilterToFunc :: VFilter -> VideoFilter
vfilterToFunc Oldest = oldest
vfilterToFunc Latest = latest


ffilterToFunc :: FFilter -> FileFilter
ffilterToFunc Unwatched = unwatched
ffilterToFunc All       = allMedia


play :: Media2 -> IO ()
play opts@(Media2{vFilter =vf, fFilter=ff, vPlayer=player, history=h, path=p}) = do
    let vfilter = vfilterToFunc vf
        fFilter = ffilterToFunc ff
    selected <- selectVideosInfo' fFilter p vfilter
    pid <- runCommand $ videoCommand player selected
    handleHistory h selected
    return ()


handleHistory :: Bool -> VideoInfo -> IO ()
handleHistory True selected = do
    t <- getCurrentTime
    addToHistory (series selected) (number selected)  (getTimeStamp  t)
    runCommand $ labelFile selected "orange"
    runCommand $ hideExtension selected
    return ()

handleHistory False _ = return ()
