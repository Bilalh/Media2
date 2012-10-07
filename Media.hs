import Media.IO
import Media.Player
import System.Process (runCommand)

path="/Users/bilalh/Movies/.Movies/Anime/"

-- Categorise videos by series and presents a menu for playing them with mplayer

main = do 
    pid <- play MPlayerOSX
    return pid

play player = do
    selected <- selectVideosInfo path oldest
    pid <- runCommand $ videoCommand player selected
    return pid