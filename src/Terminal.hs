module Terminal (getTitleLength) where
import TermSize(getTermSize)


getTitleLength :: Int -> IO Int
getTitleLength rest = do
    (height,cols) <- getTermSize
    let titleLength =  getTitleLength' (cols - rest)
    return titleLength

getTitleLength' :: Int -> Int
getTitleLength'  len
    | len > 55  = 55
    | len < 41  = 41
    | otherwise = len
