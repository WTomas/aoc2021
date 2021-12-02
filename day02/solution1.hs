readInt :: String -> Int
readInt = read

-- position stored as (x == horizontal pos, y == depth)
-- Beware, that y is inverted, as it is a measure of depth.

move :: [String] -> (Int, Int)
move ["up", x] = (0, (-1) * readInt x )
move ["down", x] = (0, readInt x )
move ["forward", x] = (readInt x, 0)
move _ = error "Invalid move!"

main :: IO()
main = do
    contents <- readFile "day02/input.txt"
    let instructions = map words $ lines contents
    let moves = map move instructions
    let position = foldl1 (\(x, y) (x', y') -> (x+x', y+y')) moves
    print . uncurry (*) $ position
