module Main where
import Control.Applicative((<$>))

import Data.Char(isSpace,isDigit)
import Data.Function(on)
import Data.List(sortBy,groupBy,intercalate,nub)
import Data.Maybe(fromMaybe,isJust,fromJust)

import Media.IO(parseName,videos, videosInfo,latest)
import Media.Types(VideoInfo(..))

import System.Environment(getArgs)
import System.FilePath(dropExtensions)

import Text.HandsomeSoup(css,parseHtml)
import Text.Printf(printf)
import Text.XML.HXT.Core

import qualified Data.Map as M


type Group = String
type Url   = String

aPath :: String
aPath="/Users/bilalh/Movies/.Movies/Anime/"

main :: IO()
main = getArgs >>= run

run :: [String] -> IO ()
run [fname] = run [fname,""]
run [fname,s] = do
    contents <- readFile fname
    let doc = parseHtml contents

    res <-  processPage doc
    let res' = groupFileNames res
    printData (s == "") False res'

run _ = return ()

printData :: Bool -> Bool ->  [[(VideoInfo, Url, Group)]] -> IO ()
printData showOnlyFollowing short res =  do
    let filtered = map (filter  (\a ->  thd3  a `notElem` ["HorribleSubs"]  )) res
    {-(putStrLn . show) res'-}

    videoes <- videos aPath >>= videosInfo
    let newest = latest videoes
    {-(putStrLn . show) newest-}

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

    printVideo (VideoInfo{number=n}, url,group ) =
        printf "   %3d: %-16s %s\n" n group url

processPage :: IOSArrow XmlTree XmlTree  ->  IO [(String, Url)]
processPage doc =
    runX  $ doc
    >>> processTopDown (filterA $ neg (hasName "wbr"))
    >>> css "div.link a"
    >>> (this >>> deep getText >. concat &&& getAttrValue "href"  )



groupFileNames :: [(String, String)] -> [[(VideoInfo, Url,Group)]]
groupFileNames info =
    let (res',groups) = unzip $ processFileNames info
        res2 = madeVideoInfo res'
        res2'= zipWith (\(a,b) c -> (a,b,c) ) res2 groups
        res3 = sortBy (compare `on` fst3) res2'
        res4 = groupBy ((==) `on` (series . fst3) ) res3
    in  res4

    where
    madeVideoInfo ::  [(String,Url)] -> [(VideoInfo,Url)]
    madeVideoInfo = map (first (arr parseName))

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
    . removeMetaData (')','(')
    . removeMetaData (']','[')
    $ rev

    where
    fp = dropExtensions s
    rev = (reverse . dropWhile isSpace) fp

    removeMetaData :: (Char,Char) -> String -> String
    removeMetaData t str =
         case removeGroup' t str of
            Nothing -> str
            Just (r,_)  -> removeMetaData t . dropWhile isSpace $ r

    removeV2 :: String -> String
    removeV2 ('2':'v':s'@(n:_)) | isDigit n = s'
    removeV2 s' = s'


