
import Data.List
import Data.List.Split
import Control.Arrow

type Board = [[Int]]

readInt :: String -> Int
readInt = read

markNumber :: Int -> Board -> Board
markNumber number board = map (map (\entry -> if number == entry then 0 else entry)) board

markNumberOnBoards :: Int -> [Board] -> [Board]
markNumberOnBoards number = map $ markNumber number 

isRowBingo :: Board -> Bool
isRowBingo board = any (== 0) $ map sum board

isColumnBingo :: Board -> Bool
isColumnBingo = isRowBingo . transpose

isBingo :: Board -> Bool
isBingo board = uncurry (||) $ (isRowBingo &&& isColumnBingo) board

evaluateBoard :: Board -> Int
evaluateBoard board = sum $ map sum board

playBingo :: [Int] -> [Board] -> (Int, Board)
playBingo [] _ = error "Ran out of draws"
playBingo _ [] = error "No unique board left"
playBingo draws boards = 
    case filter (not . isBingo) markedBoards of
        [] -> (draw, head markedBoards)
        remainingMarkedBoards -> playBingo (tail draws) remainingMarkedBoards
    where
        markedBoards = markNumberOnBoards draw boards
        draw = head draws

main :: IO()
main = do
    contents <- readFile "day04/input.txt"
    let lines' = splitOn "\n" contents
    let draws = map readInt $ splitOn "," $ head lines'
    let boards = chunksOf 5 $ map (map readInt . words) . tail $ filter (/= "") lines'
    let (winningNumber, lastBoard) = playBingo draws boards
    let scoreOfLastBoard = evaluateBoard lastBoard
    print $ scoreOfLastBoard * winningNumber
