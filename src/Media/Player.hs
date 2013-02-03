{-# LANGUAGE DeriveDataTypeable #-}

module Media.Player (
  videoCommand, PlayerType(..)
) where

import Data.Typeable
import Data.Data

import Media.Types 
import Media.Misc

data PlayerType = MPlayer | MPlayerOSX | MPlayerX | VLC | MPV
    deriving (Show, Data, Typeable)


--  command to run on the file
videoCommand ::  PlayerType -> VideoInfo -> String
videoCommand MPlayer info = 
    "mplayer -input file=/Users/bilalh/.mplayer/pipe -input conf=input_no_enter.conf -geometry 0:0 -xy 480 -really-quiet " ++   esc info  ++ " &> /dev/null"
videoCommand MPV info = 
    "mpv -geometry 0%:100% --autofit=480 " ++   esc info  ++ " &> /dev/null"
videoCommand MPlayerOSX info =  "open -a 'MPlayer OSX Extended' --args " ++ esc info  
videoCommand MPlayerX   info =  "open -a MPlayerX --args "               ++ esc info  
videoCommand VLC        info =  "open -a VLC --args "                    ++ esc info  

esc info = bashEscape  (filename info) 
