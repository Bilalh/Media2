module Media.TimeSpec where

import Test.Hspec
import Test.HUnit.Base

import Data.Time.Clock
import Data.Time
import Data.Time.Format

import System.Locale

import Media.Time

parseTimeStringWrapper :: String -> String -> IO Time
parseTimeStringWrapper timestamp  testString= do
    let utcDate = readTime defaultTimeLocale "%F %H:%M:%S" timestamp :: UTCTime
    let t = parseTimeString utcDate testString
    case t of
        Left _     -> error $ "Could not parse " ++ testString
        Right time -> return time

parseTimeAbsoluteWrapper :: String -> String -> String -> IO (UTCTime, UTCTime, UTCTime)
parseTimeAbsoluteWrapper start finish testString = do
    let start'  = readTime defaultTimeLocale "%F %H:%M:%S" start :: UTCTime
    let finish' = readTime defaultTimeLocale "%F %H:%M:%S" finish :: UTCTime
    t <- parseTimeStringWrapper "2012-10-10 01:16:37" testString
    return (start', finish', addTime start' t)

spec = do
    describe "parseTimeString relative dates" $ do
        it "returns -5 hours on 5 hours ago" $ do
            t <- parseTimeStringWrapper "2012-10-10 01:16:37" "5 hours ago"
            t @?=  time'{hours=(-5)}

    describe "parseTimeString absolute dates" $ do
        it "return 11pm when it given" $ do
            (start,finish,result) <- parseTimeAbsoluteWrapper "2012-10-10 01:16:37" "2012-10-10 23:00:00" "11pm"
            result @?= finish
