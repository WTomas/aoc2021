import Data.List.Split
import Data.List

readInt :: String -> Int
readInt = read

fuelUsage :: Int -> Int
fuelUsage distance = (distance * (distance+1)) `div` 2

main = do
    contents <- readFile "day07/input.txt"
    let positions = sort $ map readInt $ splitOn "," contents
    let possibleValues = [(minimum positions)..(maximum positions)]
    let fuelUsages = map (\possibleValue -> sum $ map (\position -> fuelUsage $ abs (position - possibleValue)) positions) possibleValues
    print $ minimum fuelUsages
