{-# LANGUAGE DeriveDataTypeable #-}

import Media.IO
import Media.Player
import Media.Types
import Media.Misc
import Media.History
import Media.Args

import Data.Time.Clock
import Data.List(foldl')
import qualified Data.Map as M

import System.Environment (getArgs)
import System.Process (runCommand)

import System.Console.CmdArgs


data VFilter = Oldest    | Latest deriving (Data, Typeable, Show)
data FFilter = Unwatched | All    deriving (Data, Typeable, Show)

data Media2 = Media2
    {vFilter    :: VFilter
    ,fFilter    :: FFilter
    ,vPlayer    :: PlayerType
    ,history    :: Bool
    ,path       :: FilePath
    ,extra_args :: [String]
    ,default_   :: Bool
    ,filter_    :: [String]
    ,screen     :: Maybe Screen
    ,fs_screen  :: Maybe FsScreen
    }
    deriving (Data, Typeable, Show)

-- Categorise videos by series and presents a menu for playing them with a player
main = do
    opts <- cmdArgs getOpts
    print opts
    opts' <- fillInOpts opts
    _ <- play opts'
    return ()


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
            [ MPlayerOSX &= name "m"  &= help "Use MPlayerOSX as the player (Default)"
            , MPlayerX   &= name "x"  &= help "Use MPlayerX as the player"
            , MPlayer    &= name "M"  &= help "Use MPlayer (command line) as the player"
            , MPV        &= name "v"  &= help "Use mpv (command line) as the player"
            , MPV_App    &= name "V"  &= help "Use mpv (app) as the player"
            , VLC                     &= help "Use VLC as the player"
            ]
        ,history    = def &= name "y" &= help "Add files to history"
        ,path       = def &= name "p" &= help "Directory to look for files includes sub dirs " &= typDir
        ,extra_args = def &= name "e" &= help "Extra args to pass to the player"
        ,default_   = def &= name "d" &= help "Use default args for command line players"
        ,filter_    = def &= args     &= typ "regex"
        ,screen     = def &= name "#" &= help "Put the player on the specifed screen"
        ,fs_screen  = def &= name "@" &= help "Put the player on the specifed screen when in fullscreen"
        } &=
        versionArg [ignore] &=
        program "media2" &=
        help "Categorise videos by series and presents a menu for playing them." &=
        summary "Media' v2.0 (C) Bilal Syed Hussain"

fillInOpts :: Media2 -> IO Media2
fillInOpts opts@(Media2{path=p} ) | p == ""  =do
    def <- defaultPath
    return opts{path=def}

fillInOpts m = return m


vfilterToFunc :: VFilter -> VideoFilter
vfilterToFunc Oldest = oldest
vfilterToFunc Latest = latest

ffilterToFunc :: FFilter -> FileFilter
ffilterToFunc Unwatched = unwatched
ffilterToFunc All       = allMedia


getDefaultArgs :: Bool ->  PlayerType -> String
getDefaultArgs True MPlayer = defaultMplayerArgs
getDefaultArgs True MPV     = defaultMpvArgs
getDefaultArgs True MPV_App = defaultMpvArgs
getDefaultArgs _ _          = ""


play :: Media2 -> IO ()
play opts@(Media2{vFilter =vf, fFilter=ff, vPlayer=player,
                  history=h, path=p, extra_args=ea, filter_=f, default_=d, screen=sc, fs_screen=fsc}) = do
    let vfilter = vfilterToFunc vf
        fFilter = ffilterToFunc ff
    selected <- selectVideosInfo' (filterPaths' f) fFilter p vfilter
    let args =  (getDefaultArgs d player ++ " ") :  mpvArgs sc : mpvArgs fsc : ea
    let command = videoCommand player [filename selected] (foldl' (\a b -> a ++ " " ++ b ) "" args)
    print command
    pid <- runCommand command
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

