{-# LANGUAGE DeriveDataTypeable #-}

import Data.Time.Clock
import Media.IO
import Media.Player
import Media.Types
import Media.Misc
import Media.History
import System.Process (runCommand)
import System.Environment (getArgs)

import qualified Data.Map as M

import System.Console.CmdArgs

path="/Users/bilalh/Movies/.Movies/Anime/"

data VFilter = Oldest    | Latest deriving (Data, Typeable, Show)
data FFilter = Unwatched | All    deriving (Data, Typeable, Show)

data Media2 = Media2 {
    vFilter :: VFilter, 
    fFilter :: FFilter, 
    vPlayer :: PlayerType,
    history :: Bool
    }
    deriving (Data, Typeable, Show)

-- Categorise videos by series and presents a menu for playing them with a player
main = do
    args <- getArgs
    opts <- cmdArgs $ getOpts
    _ <- play opts
    return ()
    -- return opts

-- Parse the command line options
getOpts :: Media2
getOpts =
    Media2 {
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
            , VLC        &= name "v"  &= help "Use VLC as the player"
            ],
        history = def   &= name "h" &= help "Add files to history"
        } &=
        program "media2" &=
        help "Categorise videos by series and presents a menu for playing." &=
        summary "Media' v2.0 (C) Bilal Syed Hussain" 



vfilterToFunc :: VFilter -> VideoFilter
vfilterToFunc Oldest = oldest
vfilterToFunc Latest = latest


ffilterToFunc :: FFilter -> FileFilter
ffilterToFunc Unwatched = unwatched
ffilterToFunc All       = allMedia


play :: Media2 -> IO ()
play opts@(Media2{vFilter =vf, fFilter=ff, vPlayer=player, history=h}) = do
    let vfilter = vfilterToFunc vf
        fFilter = ffilterToFunc ff
    selected <- selectVideosInfo' fFilter path vfilter
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
