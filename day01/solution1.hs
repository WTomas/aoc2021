{-# LANGUAGE ScopedTypeVariables #-}

readInt :: String -> Int
readInt = read

main :: IO()
main = do
    contents <- readFile "day01/input.txt"
    let depths :: [Int] = map readInt . words $ contents
    -- If depth i is less than depth i+1, True
    let zipped = zipWith (<) depths $ tail depths
    print . sum . map fromEnum $ zipped