module Main where

import System.Environment(getArgs)
import Media.NextEpisode(nextEpisode,addToPlaylist)

f :: String
f    = "/Users/bilalh/Movies/.Movies/Anime/Mondaiji-tachi ga Isekai kara Kuru Sou Desu yo - 01.mkv"

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run [f] = nextEpisode f >>= addToPlaylist
run _   = return ()

