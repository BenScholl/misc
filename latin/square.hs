import Data.List(sortBy)
import Data.Ord(comparing)

type Square = [[Maybe Int]]

main = print $ solve [
    [Just 0,  Nothing, Nothing],
    [Nothing, Nothing, Just 1 ],
    [Just 1,  Nothing, Nothing]]

solve :: Square -> Maybe Square
solve sq = if isComplete sq then Just sq
        else step . head . sortBy (comparing (length . snd)) . map withValidValues . empties $ sq
    where
        step (move, (v:vs)) = case solve (play v move sq) of
                                Just s -> Just s
                                Nothing -> step (move, vs)
        step _ = Nothing
        withValidValues move = (move, filter (isMoveValid sq move) nums)
        isComplete = all (all (/= Nothing))
        nums = map snd (zip sq [0..])

empties :: Square -> [(Int, Int)]
empties sq = do
    (row, y) <- zip sq [0..]
    (val, x) <- zip row [0..]
    case val of
        Nothing -> return (x, y)
        Just _  -> []

isMoveValid :: Square -> (Int,Int) -> Int -> Bool
isMoveValid sq (x,y) value = all valid (sq !! y) &&
                             all valid (map (!! x) sq)
    where valid = (/= Just value)

play :: Int -> (Int,Int) -> Square -> Square
play value (x,y) sq = map dorow (zip [0..] sq)
    where dorow (i, row) = if i == y then map docell (zip [0..] row) else row
          docell (i, cell) = if i == x then Just value else cell
