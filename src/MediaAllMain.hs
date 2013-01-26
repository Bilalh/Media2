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
data Opts = Opts {vFilter :: VFilter, fFilter :: FFilter, mPlayer :: PlayerType}
    deriving (Data, Typeable, Show)

-- Categorise videos by series and presents a menu for playing them with a player
main = do
    args <- getArgs
    opts <- getOpts
    _ <- play opts
    return ()
    -- return opts

-- Parse the command line options
getOpts :: IO Opts
getOpts = cmdArgs $
    Opts {
        vFilter = enum
            [ Oldest &= help "Prefer Older files (default)"
            , Latest &= help "Only play the newest file"
            ],
        fFilter = enum
            [ Unwatched &= help "Only plays file which have not been played (default)"
            , All       &= help "Plays all files"
            ],
        mPlayer = enum
            [ MPlayerOSX &= help "Use MPlayerOSX as the player (Deafult)"
            , MPlayerX   &= help "Use MPlayerX as the player"
            , MPlayer    &= help "Use MPlayer (command line) as the player"
            , VLC        &= help "Use VLC as the player"
            ]
        }


vfilterToFunc :: VFilter -> VideoFilter
vfilterToFunc Oldest = oldest
vfilterToFunc Latest = latest

ffilterToFunc :: FFilter -> FileFilter
ffilterToFunc Unwatched = unwatched
ffilterToFunc All       = allMedia

play :: Opts -> IO ()
play opts@(Opts{vFilter =vf, fFilter=ff, mPlayer=player}) = do
    let vfilter = vfilterToFunc vf
        fFilter = ffilterToFunc ff
    selected <- selectVideosInfo' fFilter path vfilter
    pid <- runCommand $ videoCommand player selected
    return ()
