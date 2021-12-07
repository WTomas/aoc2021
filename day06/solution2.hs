import Data.List
import Data.List.Split
import Control.Arrow

readInt :: String -> Int
readInt = read

nextDay :: [(Int, Int)] -> [(Int, Int)]
nextDay stateCounts = foldl (\acc state -> (state, (sum . map (\(state', count') -> if state == state' then count' else 0) $ stateCounts')):acc) [] [0..8]
    where 
        stateCounts' = foldl (\acc (state, count) -> if state == 0 then (6, count):(8, count):acc else (state-1, count):acc) [] stateCounts

nextNDays :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
nextNDays dayIdx finalDay stateCounts
    | dayIdx >= finalDay = stateCounts
    | otherwise = nextNDays (dayIdx+1) finalDay (nextDay stateCounts)

countFish :: [(Int, Int)] -> Int
countFish = foldl (\acc (_, count) -> acc+count) 0 

main = do
    contents <- readFile "day06/input.txt"
    let initialState = map (head &&& length) $ group . sort $ map readInt $ splitOn "," contents
    -- print $ nextDay $ nextDay initialState
    print $ countFish $ nextNDays 0 256 initialState