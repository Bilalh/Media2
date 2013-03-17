module Media.Types where

data VideoInfo = VideoInfo { series   :: String
                           , number   :: Int
                           , filename :: FilePath
                           } deriving (Show,Read,Eq,Ord)

data VideoData = VideoData 
    {vNumber   :: Int
    ,vPrevious :: Int
    ,vNext     :: String
    ,vSeries   :: String
    ,vFilePath :: FilePath
    } deriving (Show,Read,Eq,Ord)
