import Data.Time.Clock
import Media.IO
import Media.Player
import Media.Types
import Media.Misc
import Media.History
import System.Process (runCommand)

path="/Users/bilalh/Movies/.Movies/Anime/"

main = do 
    pid <- play MPlayerOSX
    return pid

play player = do
    selected <- selectVideosInfo' allMedia path oldest
    pid <- runCommand $ videoCommand player selected
    return ()
