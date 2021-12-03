{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import Control.Arrow
import Data.Char (digitToInt)

-- Taken from https://stackoverflow.com/a/26961027
toDec :: String -> Int
toDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

main :: IO()
main = do
    contents <- readFile "day03/input.txt"
    let transposedNumbers = transpose . lines $ contents 
    let groups = map (sortBy (\(_, a) (_, b) -> compare a b)) $ map (map (head &&& length) . group . sort) $ transposedNumbers
    let gammaRate = toDec $ map (\[_, (bit, _)] -> bit) groups
    let epsilonRate = toDec $ map (\[(bit, _), _] -> bit) groups
    print $ gammaRate * epsilonRate