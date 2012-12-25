module Media.Types where

data VideoInfo = VideoInfo { series   :: String
                           , number   :: Int
                           , filename :: FilePath
                           } deriving (Show,Eq,Ord)
