import Data.Time.Clock
import Media.IO
import Media.Player
import Media.Types
import Media.Misc
import Media.History
import System.Process (runCommand)

path="/Users/bilalh/Movies/.Movies/Anime/"

-- Categorise videos by series and presents a menu for playing them with a player
main = do 
    pid <- play MPlayerOSX
    return pid

play player = do
    selected <- selectVideosInfo path oldest
    -- pid <- runCommand $ videoCommand player selected
    t <- getCurrentTime
    addToHistory (series selected) (number selected)  (getTimeStamp  t)
    runCommand $ labelFile selected "orange"
    runCommand $ hideExtension selected
    return ()