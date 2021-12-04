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

findBingo :: [Board] -> Maybe Board
findBingo boards = 
    case filter isBingo boards of
        [] -> Nothing
        xs -> Just $ head xs

evaluateWinningBoard :: Board -> Int
evaluateWinningBoard board = sum $ map sum board

playBingo :: [Int] -> [Board] -> (Int, Board)
playBingo draws boards =
    case findBingo markedBoards of
        Nothing -> playBingo (tail draws) markedBoards
        Just board -> (draw, board)
    where 
        markedBoards = markNumberOnBoards draw boards
        draw = head draws

main :: IO()
main = do
    contents <- readFile "day04/input.txt"
    let lines' = splitOn "\n" contents
    let draws = map readInt $ splitOn "," $ head lines'
    let boards = chunksOf 5 $ map (map readInt . words) . tail $ filter (/= "") lines'
    let (lastNumber, winningBoard) = playBingo draws boards
    let scoreOfWinningBoard = evaluateWinningBoard winningBoard
    print $ scoreOfWinningBoard * lastNumber