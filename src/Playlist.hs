{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

import Media.IO
import Media.Player

import qualified Data.Map as M
import Data.List(foldl1', foldl')
import Data.Char(toLower)

import Text.Regex.TDFA

import System.Process (runCommand)
import System.Console.CmdArgs

-- Regex matcher which allows specifying options
(=~+) ::
   ( RegexMaker regex compOpt execOpt source
   , RegexContext regex source1 target )
   => source1 -> (source, compOpt, execOpt) -> target
source1 =~+ (source, compOpt, execOpt) = match (makeRegexOpts compOpt execOpt source) source1


data Playlists2 = Playlists2
    {vPlayer    :: PlayerType
    ,path       :: FilePath
    ,extra_args :: [String]
    ,default_   :: DefaultArgs
    ,chapter    :: Maybe Chapter
    ,start      :: Maybe Start
    ,filter_    :: [String]
    }
    deriving (Data, Typeable, Show)

-- To convert the flags such as chapter to a string
class MpvArgs t where mpvArgs :: t -> String

newtype Chapter     = Chapter  (Int,Int) deriving (Show, Read, Data, Typeable, Eq, Ord, Default)
newtype DefaultArgs = DefaultArgs Bool   deriving (Show, Read, Data, Typeable, Eq, Ord, Default)
newtype Start       = Start Int          deriving (Show, Read, Data, Typeable, Eq, Ord, Default)

instance (MpvArgs t) => MpvArgs (Maybe t) where 
    mpvArgs Nothing   = ""
    mpvArgs (Just n)  = mpvArgs n

instance MpvArgs Chapter where
    mpvArgs ( Chapter (a,b) ) = " --chapter " ++ show a ++ "-" ++ show b ++ " "

instance MpvArgs DefaultArgs where
    mpvArgs (DefaultArgs True)  = inputArgs ++ defaultmpvArgs
    mpvArgs (DefaultArgs False) = ""

instance MpvArgs Start where
    mpvArgs ( Start a ) = " --start " ++ show a ++ " "


inputArgs    = " -input file=/Users/bilalh/.mplayer/pipe -input conf=/Users/bilalh/.mpv/input_with_last_fm.conf -aspect 16:9 --shuffle "
defaultmpvArgs  = " -geometry 0%:100% --autofit=480 --loop=inf"



main = do
   opts <- cmdArgs getOpts
   print opts
   opts' <- fillInOpts opts
   play (processArgs opts')
   return ()


play :: Playlists2 -> IO ()
play opts@(Playlists2{vPlayer=player, path=p, extra_args=ea, filter_=f}) = do
   files  <- videos p
   let files2 = filterPaths files f
   let command = videoCommand player files2 args
   print command

   pid <- runCommand command
   return ()

   where args = foldl' (\a b -> a ++ " " ++ b ) "" ea


filterPaths :: [FilePath] ->  [String] -> [FilePath]
filterPaths  paths []  = paths
filterPaths  paths ps  = filter (match patten) paths
    where
    patten = foldl1' (\a b -> a ++ ".*" ++ b )  ps

    match patten str  = str =~+ (patten, defaultCompOpt{caseSensitive=False}, defaultExecOpt)


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
        ,start      = def &= name "s" &= help "Where to start playback from"
        ,filter_    = def &= args     &= typ "regex"
        } &=
        versionArg [ignore] &=
        program "playlists" &=
        help "Plays all videos in the specified directory including sub-directories" &=
        summary "playlists, part of Media' v2.0 (C) Bilal Syed Hussain"


processArgs :: Playlists2 -> Playlists2
processArgs args@(Playlists2{extra_args=ea, default_=d, chapter=c, start=s }) = 
    args{extra_args =  mpvArgs d : mpvArgs c : mpvArgs s : ea}

fillInOpts :: Playlists2 -> IO Playlists2
fillInOpts opts@(Playlists2{path=p} ) | p == ""  =do
    def <- defaultPath
    return opts{path=def}

fillInOpts m = return m

