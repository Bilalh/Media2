import Media.IO
import Media.Player
import System.Process (runCommand)

path="/Users/bilalh/Movies/.Movies/Anime/"


main = do 
    selected <- selectVideosInfo path latest
    pid <- runCommand $ videoCommand selected
    return pid