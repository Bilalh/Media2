{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Media.Args where

import Media.Player
import Data.List(foldl1')

import Text.Regex.TDFA
import System.Console.CmdArgs


-- Regex matcher which allows specifying options
(=~+) ::
   ( RegexMaker regex compOpt execOpt source
   , RegexContext regex source1 target )
   => source1 -> (source, compOpt, execOpt) -> target
source1 =~+ (source, compOpt, execOpt) = match (makeRegexOpts compOpt execOpt source) source1


class MpvArgs t where mpvArgs :: t -> String

newtype Screen      = Screen   Int       deriving (Show, Read, Data, Typeable, Eq, Ord, Default)
newtype FsScreen    = FsScreen Int       deriving (Show, Read, Data, Typeable, Eq, Ord, Default)
newtype Chapter     = Chapter  (Int,Int) deriving (Show, Read, Data, Typeable, Eq, Ord, Default)
newtype DefaultArgs = DefaultArgs Bool   deriving (Show, Read, Data, Typeable, Eq, Ord, Default)
newtype Start       = Start Int          deriving (Show, Read, Data, Typeable, Eq, Ord, Default)


instance (MpvArgs t) => MpvArgs (Maybe t) where 
    mpvArgs Nothing   = ""
    mpvArgs (Just n)  = mpvArgs n

instance MpvArgs Screen where
    mpvArgs (Screen num) = " --screen=" ++ show num

instance MpvArgs FsScreen where
    mpvArgs (FsScreen num) = " --fs-screen=" ++ show num

instance MpvArgs Chapter where
    mpvArgs ( Chapter (a,b) ) = " --chapter " ++ show a ++ "-" ++ show b ++ " "

instance MpvArgs DefaultArgs where
    mpvArgs (DefaultArgs True)  = inputArgs ++ defaultmpvArgs
    mpvArgs (DefaultArgs False) = ""

instance MpvArgs Start where
    mpvArgs ( Start a ) = " --start " ++ show a ++ " "

defaultmpvArgs  = " --shuffle  -geometry 0%:100% --autofit=480 --aspect=16:9 --loop=inf -input conf=/Users/bilalh/.mpv/input_with_last_fm.conf"

filterPaths' :: [String] ->  [FilePath] -> [FilePath]
filterPaths' = flip filterPaths

filterPaths :: [FilePath] ->  [String] -> [FilePath]
filterPaths  paths []  = paths
filterPaths  paths ps  = filter (match patten) paths
    where
    patten = foldl1' (\a b -> a ++ ".*" ++ b )  ps

    match patten str  = str =~+ (patten, defaultCompOpt{caseSensitive=False}, defaultExecOpt)

