
import Data.List




-- Given an n size NxN board give all permuations of board without repeats in rows, and no repeated rows in boards.
-- returns Array of 2D arrays
findAllBoards :: Int -> IO [[[Int]]]
findAllBoards n = do
    rows <- return (permutations [1..n])
    boards <- return (choose n rows)
    return boards

-- choose n list returns all permuations of list with length n

-- concatMap :: (a -> [b]) -> [a] -> [b]
-- Map a function over a list and concatenate the results.

-- Chooseh creates list of lists length n containing each combination of elements 
-- chooseh [1,2,3,4] [] with n=2
-- [[2,1],[3,1],[4,1],[3,2],[4,2],[4,3]]


choose n list = concatMap permutations (chooseh list [])
    where
        chooseh []     r = if length r == n then [r] else []
        chooseh (x:xs) r 
            | length r == n = [r]
            | otherwise     = chooseh xs (x:r)++ chooseh xs r


choosetest []     r n = if length r == n then [r] else []
choosetest (x:xs) r n 
    | length r == n = [r]
    | otherwise     = choosetest xs (x:r) n ++ choosetest xs r n