{-# LANGUAGE DeriveDataTypeable  #-}

import Media.IO
import Media.Player
import Media.Args

import qualified Data.Map as M
import Data.List(foldl')

import System.Process (runCommand)
import System.Console.CmdArgs

import qualified Algorithms.NaturalSort as NS

import Data.List(sortBy)

data Playlists2 = Playlists2
    {vPlayer    :: PlayerType
    ,path       :: FilePath
    ,extra_args :: [String]
    ,default_   :: DefaultArgs
    ,chapter    :: Maybe Chapter
    ,screen     :: Maybe Screen
    ,fs_screen  :: Maybe FsScreen
    ,start      :: Maybe Start
    ,filter_    :: [String]
    }
    deriving (Data, Typeable, Show)

main = do
   opts <- cmdArgs getOpts
   {-print opts-}
   opts' <- fillInOpts opts
   play (processArgs opts')
   return ()


play :: Playlists2 -> IO ()
play opts@(Playlists2{vPlayer=player, path=p, extra_args=ea, filter_=f}) = do
   files  <- videos p
   let files2 = sortBy NS.compare $ filterPaths files f
   let command = videoCommand player files2 args

   pid <- runCommand command
   return ()

   where args = foldl' (\a b -> a ++ " " ++ b ) "" ea

getOpts :: Playlists2
getOpts =
    Playlists2{
        vPlayer = enum
            [ MPV        &= name "v"  &= help "Use mpv (command line) as the player (Default)"
            , MPV_App    &= name "V"  &= help "Use mpv (app) as the player"
            , MPlayer    &= name "M"  &= help "Use MPlayer (command line) as the player"
            ]
        ,path       = def &= name "p" &= help "Directory to look for files includes sub dirs " &= typDir
        ,extra_args = def &= name "e" &= help "Extra args to pass to the player"
        ,default_   = def &= name "d" &= help "use default mpv args ( -geometry 0%:100% --autofit=480 --loop=inf) "
        ,chapter    = def &= name "c" &= help "Only play the specified chapters"
        ,screen     = def &= name "#" &= help "Put the player on the specifed screen"
        ,fs_screen  = def &= name "@" &= help "Put the player on the specifed screen when in fullscreen"
        ,start      = def &= name "s" &= help "Where to start playback from"
        ,filter_    = def &= args     &= typ "regex"
        } &=
        versionArg [ignore] &=
        program "playlists" &=
        help "Plays all videos in the specified directory including sub-directories" &=
        summary "playlists, part of Media' v2.0 (C) Bilal Syed Hussain"


processArgs :: Playlists2 -> Playlists2
processArgs args@(Playlists2{extra_args=ea, default_=d, chapter=c, start=s, screen=sc, fs_screen=fsc  }) =
    args{extra_args =   mpvArgs d : mpvArgs c : mpvArgs s : mpvArgs sc : mpvArgs fsc : ea}

fillInOpts :: Playlists2 -> IO Playlists2
fillInOpts opts@(Playlists2{path=p} ) | p == ""  =do
    def <- defaultPath
    return opts{path=def}

fillInOpts m = return m
