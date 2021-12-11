import Data.List
import Data.List.Split

readInt :: String -> Int
readInt = read


median :: [Int] -> Int
median values 
    | length values `mod` 2 == 1 = sortedValues !! (middleIdx)
    | otherwise = ((sortedValues !! middleIdx) + (sortedValues !! (middleIdx-1))) `div` 2
    where 
        sortedValues = sort values
        middleIdx = (length values) `div` 2


main = do
    contents <- readFile "day07/input.txt"
    let positions = sort $ map readInt $ splitOn "," contents
    let median' = median positions
    print $ sum $ map (\pos -> abs(pos - median')) positions