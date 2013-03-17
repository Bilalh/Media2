module Main where

import Text.XML.HXT.Core
import Media.IO(getVideoData,latest,allMedia)
import Media.History(unwatched)
import Text.Groom
import Media.Types

data Item = Item 
    {attributes :: [(String,String)]
    ,title      :: String
    ,subtitle   :: Maybe String
    } deriving (Show,Read,Eq,Ord)

class Itemable a where
    toItem :: a -> Item

instance Itemable Item where
    toItem = id

instance Itemable VideoData where
    toItem vd@VideoData{vSeries=s} = Item
          {attributes=[("uid","Media2Video" ), ("arg", n )], 
           title=s ++ " " ++ n, 
           subtitle=Just $ "Previous: " ++ p ++ "    Next: " ++ e
          }

        where 
        n = (show . vNumber) vd
        p = (show . vPrevious) vd 
        e = vNext vd 

main :: IO ()
main = return ()

process = do 
    datas <- getVideoData id unwatched "/Users/bilalh/Movies/.Movies/Anime" latest
    items <- mapM  (\a ->  xmlToString  (itemXml a) ) datas
    return $ "<items>" ++ (concat items) ++ "</items>"


itemXml :: (ArrowXml a, Itemable i) =>  i ->  a XmlTree XmlTree
itemXml i = selem "" [ 
        mkelem "item" (map (uncurry sattr) attrs ) $ 
        [ selem "title" [txt t] ]
        ++ convert "subtitle" st
        ++ [mkelem "icon" [sattr "type" "filetype"] [txt "public.mpeg-4"] ]
    ]

    where 
    Item{attributes=attrs, title=t, subtitle=st} = toItem i 
    convert name (Just text) = [ selem name [txt text]]
    convert _  Nothing       = []


xmlToString :: IOSLA (XIOState ()) XmlTree XmlTree -> IO String
xmlToString func= do
    a <- runX (func   >>> writeDocumentToString [])
    return $ concat a

_x :: Show a => a  -> IO ()
_x = putStrLn . groom 

{-
<items>

  <item uid="Shinsekai Yori" autocomplete="Shinsekai Yori" arg="21">
    <title>Shinsekai Yori 23</title>
    <subtitle>Previous: 23    Next: 23-24</subtitle>
    <icon>95085.jpg</icon>
  </item>

</items>
-}

ii  = Item{attributes=[("uid","Shinsekai Yori" ), ("arg","21")], 
           title="Shinsekai Yori", 
           subtitle=Just "Previous: 23    Next: 23-24"
          }

