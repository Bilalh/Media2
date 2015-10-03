module Main where

import System.Environment(getArgs)
import Media.NextEpisode(nextEpisodes,addToPlaylist)

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run [x] = nextEpisodes x >>= addToPlaylist
run _   = return ()


