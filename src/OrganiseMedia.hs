{-# LANGUAGE OverloadedStrings #-}
import Media.IO(getVideosInfo',allMedia,media)
import Data.List(groupBy,sortBy)
import Data.Ord(comparing)
import Media.Types(VideoInfo(..))
import System.FilePath((</>),takeBaseName, (<.>),takeExtension, splitDirectories)
import System.Directory(createDirectoryIfMissing,renameFile,doesFileExist,copyFile, canonicalizePath)
import System.Environment (getArgs)

import Crypto.Hash(Digest,hash,MD5)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.List as L
import Data.Text(Text)
import System.Posix.Files(getFileStatus, deviceID)


main = do
    [inPath,outPath] <- getArgs
    organiseMedia inPath outPath

organiseMedia inPath outPath = do
    infos <- getVideosInfo' media id allMedia inPath
    let infos'  =  map processInfos infos

    let grouped = groupBy (\a b-> series a == series b) . sortBy (comparing  series) $ infos

    createDirectoryIfMissing True outPath
    sameDrive <- sameDevice inPath outPath
    let mvFunc = case sameDrive of
            True  -> ("Moving ", renameFile)
            False -> ("Copying ", copyFile)

    mapM_ (perGroup mvFunc inPath outPath) grouped


    where
        processInfos v =
            v{series =  processSeries (series v) }

        processSeries = T.unpack . (compose $ map rmText ["v2"]) . T.pack

sameDevice :: FilePath -> FilePath -> IO Bool
sameDevice a b = do
    rs <- mapM canonicalizePath [a,b]
    fs <- mapM getFileStatus rs
    let di = map deviceID fs
    return $ all (== head di) $ di


rmText :: Text -> Text -> Text
rmText text f  =
    case (T.breakOn text f) of
        (s, "") -> f
        (s, _)  -> s

compose :: [a -> a] -> (a -> a)
compose = foldl (flip (.)) id

perGroup _ _ _ [] = return ()
perGroup mvFunc inPath outPath xx@(VideoInfo{series=series}:_) = do
    let newPath = outPath </> series
    createDirectoryIfMissing True newPath
    mapM_ (moveToDir mvFunc newPath) xx


moveToDir (mvName,mvFunc) outDir (VideoInfo{filename=fp}) = do
    let baseName = takeBaseName fp
        outPath  = outDir </> baseName <.> (takeExtension fp)

    e <- doesFileExist outPath
    -- hSame <- md5Same fp outPath
    let hSame =  False

    case (hSame, outPath == fp, e) of
        (True , _    , _)     -> return ()
        (_    , True , _)     -> return ()
        (_    , _    , True)  -> putStrLn $ "Not " ++ mvName ++ "(dst exists) " ++ fp ++ " to " ++ outPath
        (_    , _    , False) -> do
            putStrLn $ mvName ++ fp ++ " to " ++ outPath
            mvFunc fp outPath

md5Same :: FilePath -> FilePath -> IO Bool
md5Same fin fout = do
    b <- doesFileExist fout
    case b of
        False -> return False
        True -> do
            hIn  <- md5File fin
            hOut <- md5File fout
            return $ hIn == hOut

md5File :: FilePath -> IO (Digest MD5)
md5File fp = do
    byteStr <- BS.readFile fp
    return $ hash byteStr
