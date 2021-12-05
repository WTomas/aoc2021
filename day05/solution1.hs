{-# LANGUAGE ScopedTypeVariables #-}

import Data.List.Split
import Data.List

readInt :: String -> Int
readInt = read

type Point = (Int, Int)
type Line = (Point, Point)

toTuple :: [a] -> (a, a)
toTuple [x, y] = (x, y)
toTuple _ = error "Not dealing with different input types for now."

isLineStraight :: Line -> Bool
isLineStraight ((x, y), (x', y')) = x == x' || y == y'

getLinePoints :: Line -> [Point]
getLinePoints ((x, y), (x', y')) 
    | x == x' = map (\y'' -> (x, y'')) [yLow..yHigh]
    | y == y' = map (\x'' -> (x'', y)) [xLow..xHigh]
    | otherwise = []
    where
        [yLow, yHigh] = sort [y, y']
        [xLow, xHigh] = sort [x, x']

main :: IO()
main = do
    contents <- readFile "day05/input.txt"
    let lines' :: [Line] = map (toTuple . (map (toTuple . map readInt . splitOn ",")) . (splitOn "->")) $ lines contents
    let straightLines = filter isLineStraight lines'
    print $ length . filter (>=2) $ map length $ group . sort . concat $ map getLinePoints straightLines