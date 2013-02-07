{-# LANGUAGE DeriveDataTypeable #-}

module Media.Player (
  videoCommand, PlayerType(..)
) where

import Data.Typeable
import Data.Data

import Media.Types
import Media.Misc

data PlayerType = MPlayer | MPlayerOSX | MPlayerX | VLC | MPV | MPV_App
    deriving (Show, Data, Typeable)



defaultArgs        =  "-input file=/Users/bilalh/.mplayer/pipe -input conf=input_no_enter.conf"
defaultMplayerArgs = defaultArgs ++  "-geometry 0:0 -xy 480 -really-quiet "
defaultMpvArgs     = defaultArgs ++ " -geometry 0%:100% --autofit=480 "

--  command to run on the file
--  CHANGE pipe to location of mplayer's pipe
videoCommand ::  PlayerType -> VideoInfo -> String -> String
videoCommand MPlayer info extraArgs =
    "mplayer "                      ++ defaultMplayerArgs ++ extraArgs ++ " " ++ esc info ++ " &> /dev/null"
videoCommand MPV info extraArgs =
    "mpv "                          ++ defaultMpvArgs     ++ extraArgs ++ " " ++ esc info ++ " &> /dev/null"
videoCommand MPV_App info extraArgs =
    "open -a mpv --args "           ++ defaultMpvArgs     ++ extraArgs ++ " " ++ esc info ++ " &> /dev/null"

videoCommand MPlayerOSX info  _ =  "open -a 'MPlayer OSX Extended' --args " ++ esc info
videoCommand MPlayerX   info  _ =  "open -a MPlayerX --args "               ++ esc info
videoCommand VLC        info  _ =  "open -a VLC --args "                    ++ esc info

esc info = bashEscape  (filename info)
