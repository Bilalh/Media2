module Media.Misc where

import qualified Data.ByteString.UTF8 as B
import qualified Text.ShellEscape as Esc

import Data.Time.Format
import Data.Time.Clock

parseInt :: String -> Maybe Int
parseInt s  =  case reads s :: [(Int,String)] of
            [(d,"")] -> Just d
            otherwise -> Nothing

parseIntCrash :: String -> Int
parseIntCrash  s = case reads s :: [(Int,String)] of
            [(d,"")]  -> d
            otherwise -> error "Not a int"

getTimeStamp :: UTCTime -> String
getTimeStamp t =
    let res = span (/= '.') $ show t
    in fst res

bashEscape :: String -> String
bashEscape  = B.toString . Esc.bytes . Esc.bash . B.fromString
