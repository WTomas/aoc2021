{-# LANGUAGE ScopedTypeVariables #-}

readInt :: String -> Int
readInt = read 

main :: IO()
main = do
    contents <- readFile "day1/input.txt"
    let depths :: [Int] = map readInt . words $ contents
    let depths3 = map (\(x, y, z) -> x+y+z) $ zip3 depths (tail depths) (tail . tail $ depths)
    let zipped = zipWith (<) depths3 $ tail depths3
    print . sum . map fromEnum $ zipped

