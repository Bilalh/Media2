module Main where
import Control.Applicative((<$>))

import Data.Char(isSpace,isDigit)
import Data.Function(on)
import Data.List(sortBy,groupBy,intercalate,nub,isInfixOf)
import Data.Maybe(fromMaybe,isJust,fromJust)

import Media.IO(parseName,videos, videosInfo,latest)
import Media.Types(VideoInfo(..))

import System.Environment(getArgs)
import System.FilePath(dropExtension,takeExtension)

import Text.HandsomeSoup(css,parseHtml)
import Text.Printf(printf)
import Text.XML.HXT.Core
import Text.Groom(groom)

import Network.HTTP(urlEncodeVars)

import qualified Data.Map as M

type Group = String
type Url   = String

aPath :: String
aPath="/Users/bilalh/Movies/.Movies/Anime/"

main :: IO()
main = getArgs >>= run

run :: [String] -> IO ()
run [fname,fname2] = run [fname,fname2,""]
run [fname,fname2,s] = do

    res <- mapM process [fname,fname2]
    let res' = groupFileNames (concat res)
    --(putStrLn . groom . last ) res'

    let res'' = map nubSame res'
    printData (s == "") False res''
    return ()

    where
    process :: FilePath -> IO [(String,Url)]
    process f = do
        contents <- readFile f
        let doc = parseHtml contents
        processPage doc


    nubSame :: [(VideoInfo,Url,Group)] -> [(VideoInfo,Url,Group)]
    nubSame arr = concatMap reduce grouped

        where
        grouped = groupBy groupCon arr

        groupCon (VideoInfo{number=n1},_,g1) (VideoInfo{number=n2},_,g2) =
            n1 == n2 && g1 == g2

        reduce :: [(VideoInfo,Url,Group)] -> [(VideoInfo,Url,Group)]
        reduce = reduce' ["bit"] . reduce' ["480","1080"]

        reduce' :: [String] ->  [(VideoInfo,Url,Group)] ->  [(VideoInfo,Url,Group)]
        reduce' [] ar@[_,_] = case filter (reducer "v2" ) ar of
                        [] -> ar
                        [x] -> [x] 

        reduce' (x:xs) ar = case filter (not . reducer x) ar of
                        []  -> ar
                        [x] -> [x]
                        ar' -> reduce' xs ar'

        reduce' [] ar = ar

        reducer :: String -> (VideoInfo,Url,Group) -> Bool
        reducer sub (VideoInfo{filename=fn},_,_) =  sub `isInfixOf` fn

run _ = return ()


printData :: Bool -> Bool ->  [[(VideoInfo, Url, Group)]] -> IO ()
printData showOnlyFollowing short res =  do
    let filtered = map (filter  (\a ->  thd3  a `notElem` ["HorribleSubs"]  )) res
    --(putStrLn . groom) filtered

    videoes <- videos aPath >>= videosInfo
    let newest = latest videoes
    --(putStrLn . show) newest

    let filtered2 = map (onlyFollowing newest) filtered
        res' = if showOnlyFollowing then filtered2 else filtered
        func = if short then printShort else printGroup  newest

    mapM_ func res'



onlyFollowing :: M.Map String VideoInfo ->  [(VideoInfo, Url, Group)] ->  [(VideoInfo, Url, Group)]
onlyFollowing _ [] = []
onlyFollowing mapping ar@( (VideoInfo{series=name},_,_):_)
    | let info = M.lookup name mapping
    , isJust info =
      let VideoInfo{number=current} = fromJust info
      in filter (\(VideoInfo{number=m},_,_) ->  m > current ) ar


onlyFollowing _ _ = []


printShort :: [(VideoInfo, Url, Group)] -> IO ()
printShort [] = return ()
printShort ar =
    putStrLn $ seriesName ++ " : " ++ intercalate ", " nums

    where
    VideoInfo{series = seriesName} = fst3 . head $ ar
    nums = nub $ map (\(VideoInfo{number=n},_,_) -> show n ) ar


printGroup :: M.Map String VideoInfo -> [(VideoInfo, Url, Group)] -> IO ()
printGroup _ [] = return ()
printGroup vmap ar = do
    putStrLn $ seriesName ++ " : " ++ intercalate ", " nums ++ have
    mapM_ printVideo ar
    putStrLn " "

    where
    VideoInfo{series = seriesName} = fst3 . head $ ar
    nums = map (\(VideoInfo{number=n},_,_) -> show n ) ar
    have = fromMaybe "" $ (++) " \tHave " . show . number <$> M.lookup seriesName vmap

    printVideo :: (VideoInfo,Url,String) -> IO ()
    printVideo (VideoInfo{number=n,filename=fn}, url,group ) = do
        printf "   %3d: %-16s %s\n" n group url
        printf "   %3s  %-16s %s\n" "" "" (nibl fn)

    nibl :: FilePath -> Url
    nibl fn= "http://nibl.co.uk/bots.php?" ++
        urlEncodeVars [ ("sortby","filename"),("order","asc"), ("search",fn) ]

processPage :: IOSArrow XmlTree XmlTree  ->  IO [(String, Url)]
processPage doc =
    runX  $ doc
    >>> processTopDown (filterA $ neg (hasName "wbr"))
    >>> css "div.link a"
    >>> (this >>> deep getText >. concat &&& getAttrValue "href"  )


groupFileNames :: [(String, Url)] -> [[(VideoInfo, Url,Group)]]
groupFileNames info =
         groupBy ((==) `on` series . fst3 )
       . sortBy (compare `on` fst3)
       . zipWith (\c (a,b)  -> (a,b,c) ) groups
       . zipWith giveRealFileName info
       . madeVideoInfo
       $ res'

    where
    (res',groups) = unzip $ processFileNames info

    madeVideoInfo ::  [(String,Url)] -> [(VideoInfo,Url)]
    madeVideoInfo = map $ first (arr parseName)

    giveRealFileName :: (String,Url) -> (VideoInfo,Url)  -> (VideoInfo,Url)
    giveRealFileName (fn,url) (v, _)  = (v{filename=fn},url)

    processFileNames :: [(String,b)] -> [((String, b), Group)]
    processFileNames = map f
    f :: (String, t) -> ((String,t), Group)
    f (a,b) =
        let (s,group) = processFileName a
        in ((s,b),group)


fst3 :: (a,b,c) -> a
fst3 (x,_,_) =  x

thd3 :: (a,b,c) -> c
thd3 (_,_,x) =  x

processFileName :: String -> (String,Group)
processFileName =
      first (arr processEnd . dropWhile isSpace )
    . removeGroup ('[',']')
    . dropWhile isSpace
    . map _ToSpace
    . dropWhile isSpace

    where
    _ToSpace '_' = ' '
    _ToSpace c = c

    removeGroup :: (Char,Char) -> String -> (String,Group)
    removeGroup ab s = fromMaybe (s,"") (removeGroup' ab s)

removeGroup' :: (Char, Char) -> String -> Maybe (String,Group)
removeGroup' (a,b) (x:xs) | x == a =
    case dropWhile (/= b) xs of
      ""       -> Nothing
      (c:rest) -> if c /= b then Nothing else  Just (rest, takeWhile (/=b) xs )

removeGroup' _ _ = Nothing

processEnd :: String -> String
processEnd s =
      reverse
    . removeV2
    . dropWhile isSpace
    . removeMetaData (')','(')
    . dropWhile isSpace
    . removeMetaData (']','[')
    . dropWhile isSpace
    . removeMetaData (')','(')
    $ rev

    where
    fp = if  takeExtension s `elem` [".mkv",".mp4","avi",".webm"]  then dropExtension s else s
    rev = (reverse . dropWhile isSpace) fp

    removeMetaData :: (Char,Char) -> String -> String
    removeMetaData t str =
         case removeGroup' t str of
            Nothing -> str
            Just (r,_)  -> removeMetaData t . dropWhile isSpace $ r

    removeV2 :: String -> String
    removeV2 ('2':'v':s'@(n:_)) | isDigit n = s'
    removeV2 s' = s'


