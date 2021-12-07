import Data.List.Split

readInt :: String -> Int
readInt = read

iterateState :: Int -> Int -> [Int] -> [Int]
iterateState dayIdx dayLimit states
    | dayIdx >= dayLimit = states
    | otherwise = iterateState (dayIdx+1) dayLimit (foldl (\acc state -> if state == 0 then 6:8:acc else (state-1):acc) [] states)

main = do
    contents <- readFile "day06/input.txt"
    let initialState = map readInt $ splitOn "," contents
    print $ length $ iterateState 0 80 initialState