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

defaultmpvArgs  = "-aspect 16:9 --shuffle  -geometry 0%:100% --autofit=480 --loop=inf"

filterPaths' :: [String] ->  [FilePath] -> [FilePath]
filterPaths' = flip filterPaths

filterPaths :: [FilePath] ->  [String] -> [FilePath]
filterPaths  paths []  = paths
filterPaths  paths ps  = filter (match patten) paths
    where
    patten = foldl1' (\a b -> a ++ ".*" ++ b )  ps

    match patten str  = str =~+ (patten, defaultCompOpt{caseSensitive=False}, defaultExecOpt)

