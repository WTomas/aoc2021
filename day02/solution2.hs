
readInt :: String -> Int
readInt = read

measureAim :: Int -> [String] -> Int
measureAim previousAim ["up", x] = previousAim - readInt x
measureAim previousAim ["down", x] = previousAim + readInt x
measureAim previousAim ["forward", _] = previousAim
measureAim _ _ = error "Invalid move!"

type Position = (Int, Int)

move :: ([String], Int) -> Position
move (["up", _], _) = (0, 0)
move (["down", _], _) = (0, 0)
move (["forward", x], aim) = (x', aim * x') where
    x' = readInt x
move _ = error "Invalid move!"

main :: IO()
main = do
    contents <- readFile "day02/input.txt"
    let moves = map words $ lines $ contents
    let aims = scanl measureAim 0 $ moves
    let movesWithAims = zip moves aims
    let coordinateMoves = map move movesWithAims
    let finalPosition = foldl1 (\(x, y) (x', y') -> (x+x', y+y')) coordinateMoves
    print . uncurry (*) $ finalPosition
