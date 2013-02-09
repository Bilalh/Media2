{-# LANGUAGE DeriveDataTypeable #-}

import Media.IO
import Media.Player

import qualified Data.Map as M

import System.Process (runCommand)
import System.Console.CmdArgs

data Playlists2 = Playlists2 {
    vPlayer   :: PlayerType,
    path      :: FilePath,
    extraArgs :: String,
    nodefault :: Bool,
    chapter   :: Maybe ((Int,Int))
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
play opts@(Playlists2{vPlayer=player, path=p, extraArgs=ea}) = do
   files <- videos p

   let command = videoCommand (player) files ea
   print command

   pid <- runCommand command
   return ()


getOpts :: Playlists2
getOpts =
    Playlists2{
        vPlayer = enum 
            [ MPV        &= name "v"  &= help "Use mpv (command line) as the player (Default)"
            , MPV_App    &= name "V"  &= help "Use mpv (app) as the player"
            , MPlayer    &= name "M"  &= help "Use MPlayer (command line) as the player"
            ]
        ,path      = def &= name "p" &= help "Directory to look for files includes sub dirs " &= typDir
        ,extraArgs = def &= name "e" &= help "Extra args to pass to the player"
        ,nodefault = def &= name "n" &= help "Don't use default mpv args ( -geometry 0%:100% --autofit=480 --loop=inf) "
        ,chapter   = def &= name "c" &= help "Only play the specifed chapters"
        } &=
        versionArg [ignore] &=
        program "playlists" &=
        help "Plays all videos in the specified directory including sub-directories" &=
        summary "part of Media' v2.0 (C) Bilal Syed Hussain"


processChapter :: Playlists2 -> Playlists2
processChapter args@(Playlists2{chapter=Just (a,b), extraArgs=ea})  = 
    args{extraArgs=" --chapter " ++ (show a) ++ "-" ++ (show b) ++ " " ++ ea}

processChapter args = args

processNodefault :: Playlists2 -> Playlists2
processNodefault args@(Playlists2{nodefault=False, extraArgs=ea})  = 
    args{extraArgs=playlistArgs ++ argss ++ ea}

processNodefault args = args 

fillInOpts :: Playlists2 -> IO Playlists2
fillInOpts opts@(Playlists2{path=p} ) | p == ""  =do
    def <- defaultPath
    return opts{path=def}

fillInOpts m = return m

