import Data.List
import Control.Arrow
import Data.Char (digitToInt)

-- Taken from https://stackoverflow.com/a/26961027
toDec :: String -> Int
toDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

getIthBitFrequencies :: Int -> [String] -> [(Char, Int)]
getIthBitFrequencies bitIdx readings = sortBy (\(bit, count) (bit', count') -> compare (count, bit) (count', bit')) $ map (head &&& length) $ group . sort $ (transpose readings) !! bitIdx

getOxygenGeneratorRating :: Int -> [String] -> [String]
getOxygenGeneratorRating _ [] = error "Filtered everything :("
getOxygenGeneratorRating _ [x] = [x]
getOxygenGeneratorRating bitIdx readings = getOxygenGeneratorRating (bitIdx+1) (filter (\binaryReading -> (binaryReading !! bitIdx) == mostCommonBit ) readings) where
    mostCommonBit = fst . last $ getIthBitFrequencies bitIdx readings

getCO2ScrubberRating :: Int -> [String] -> [String]
getCO2ScrubberRating _ [] = error "Filtered everything :("
getCO2ScrubberRating _ [x] = [x]
getCO2ScrubberRating bitIdx readings = getCO2ScrubberRating (bitIdx+1) (filter (\binaryReading -> (binaryReading !! bitIdx) == mostCommonBit ) readings) where
    mostCommonBit = fst . head $ getIthBitFrequencies bitIdx readings

main :: IO()
main = do
    contents <- readFile "day03/input.txt"
    let readings = lines $ contents
    let oxygenGeneratorRating = toDec . head $ getOxygenGeneratorRating 0 readings
    let cO2ScrubberRating = toDec . head $ getCO2ScrubberRating 0 readings
    print $ oxygenGeneratorRating * cO2ScrubberRating