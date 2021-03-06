{-# LANGUAGE DeriveDataTypeable #-}

module Media.Player (
  videoCommand, PlayerType(..),
  defaultMplayerArgs,
  defaultMpvArgs,
  inputArgs
) where

import Data.Typeable
import Data.Data
import Data.List(foldl1')

import Media.Types
import Media.Misc

data PlayerType = MPlayer | MPlayerOSX | MPlayerX | VLC | MPV | MPV_App
    deriving (Show, Data, Typeable)


inputArgs          = " -input file=/Users/bilalh/.mplayer/pipe "
defaultMplayerArgs = inputArgs ++  " -geometry 0:0 -xy 480 -really-quiet "
defaultMpvArgs     = inputArgs ++  " -geometry 0%:100% --autofit=480 "

--  command to run on the file
--  CHANGE pipe to location of mplayer's pipe
videoCommand ::  PlayerType -> [FilePath] -> String -> String
videoCommand MPlayer info extraArgs =
    "/usr/local/bin/mplayer "                        ++ extraArgs ++ " " ++ esc info ++ " > /Users/bilalh/.mplayer/output"
videoCommand MPV info extraArgs =
    "/usr/local/bin/mpv "                            ++ extraArgs ++ " " ++ esc info ++ " > /Users/bilalh/.mplayer/output"
videoCommand MPV_App info extraArgs =
    "open -a mpv --args "             ++ extraArgs ++ " " ++ esc info ++ " &> /dev/null"

videoCommand MPlayerOSX info  _ =  "open -a 'MPlayer OSX Extended' --args " ++ esc info
videoCommand MPlayerX   info  _ =  "open -a MPlayerX --args "               ++ esc info
videoCommand VLC        info  _ =  "open -a VLC --args "                    ++ esc info

esc []  = error "No video files found!"
--esc [x] = bashEscape x
--esc infos = foldl1' (\a b -> a ++ " " ++ bashEscape b) infos
esc [info] = foldl1' (\a b -> a ++ " " ++ b)  (map ( f . bashEscape) [info])
    where
    f a = a ++ " --sub-file " ++ a

esc infos = foldl1' (\a b -> a ++ " " ++ b)  (map (  bashEscape) infos)
