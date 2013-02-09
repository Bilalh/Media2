{-# LANGUAGE DeriveDataTypeable #-}

import Media.IO
import Media.Player

import qualified Data.Map as M
import Data.List(foldl1', foldl')
import Data.Char(toLower)

import Text.Regex.TDFA

import System.Process (runCommand)
import System.Console.CmdArgs

(=~+) ::
   ( RegexMaker regex compOpt execOpt source
   , RegexContext regex source1 target )
   => source1 -> (source, compOpt, execOpt) -> target
source1 =~+ (source, compOpt, execOpt)
  = match (makeRegexOpts compOpt execOpt source) source1

data Playlists2 = Playlists2
    {vPlayer   :: PlayerType
    ,path      :: FilePath
    ,extra_args :: [String]
    ,no_default :: Bool
    ,chapter   :: Maybe ((Int,Int))
    ,filter_    :: [String]
    }
    deriving (Data, Typeable, Show)

playlistArgs = " -input file=/Users/bilalh/.mplayer/pipe -input conf=/Users/bilalh/.mpv/input_with_last_fm.conf -aspect 16:9 --shuffle "
argss        = " -geometry 0%:100% --autofit=480 --loop=inf"


main = do
   opts <- cmdArgs $ getOpts
   print opts
   opts' <- fillInOpts opts
   play (processChapter $ processNodefault opts')
   return ()


play :: Playlists2 -> IO ()
play opts@(Playlists2{vPlayer=player, path=p, extra_args=ea, filter_=f}) = do
   files  <- videos p
   let files2 = filterPaths files f
   let command = videoCommand (player) files2 (foldl' (\a b -> a ++ " " ++ b ) "" ea)
   --print command

   pid <- runCommand command
   return ()


filterPaths :: [FilePath] ->  [String] -> [FilePath]
filterPaths  paths []  = paths
filterPaths  paths ps  = filter (match patten) paths

    where
    patten = foldl1' (\a b -> a ++ ".*" ++ b ) $ ps

    match patten str  = str =~+ (patten, defaultCompOpt{caseSensitive=False}, defaultExecOpt)  :: Bool


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
        ,no_default = def &= name "n" &= help "Don't use default mpv args ( -geometry 0%:100% --autofit=480 --loop=inf) "
        ,chapter    = def &= name "c" &= help "Only play the specifed chapters"
        ,filter_    = def &= args &= typ "regex"
        } &=
        versionArg [ignore] &=
        program "playlists" &=
        help "Plays all videos in the specified directory including sub-directories" &=
        summary "playlists, part of Media' v2.0 (C) Bilal Syed Hussain"


processChapter :: Playlists2 -> Playlists2
processChapter args@(Playlists2{chapter=Just (a,b), extra_args=ea})  =
    args{extra_args= (" --chapter " ++ (show a) ++ "-" ++ (show b) ++ " ") : ea}

processChapter args = args

processNodefault :: Playlists2 -> Playlists2
processNodefault args@(Playlists2{no_default=False, extra_args=ea})  =
    args{extra_args=(playlistArgs ++ argss)  :ea}

processNodefault args = args

fillInOpts :: Playlists2 -> IO Playlists2
fillInOpts opts@(Playlists2{path=p} ) | p == ""  =do
    def <- defaultPath
    return opts{path=def}

fillInOpts m = return m

