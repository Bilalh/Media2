import Media.IO
import Media.Player
import System.Process (runCommand)

path="/Users/bilalh/Movies/.Movies/Anime/"

-- Categorise videos by series and presents a menu for playing them with mplayer

main = do 
    selected <- selectVideosInfo path latest
    pid <- runCommand $ videoCommand selected
    return pid