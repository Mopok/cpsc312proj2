
import Data.List
import Text.Printf
import Data.List
import CheckSolutions





{-
   getSolution ([0,1,2,0],[0,0,0,2],[3,0,3,0],[0,0,0,0]) 4
   getSolution ([])
-}
-- getSolution :: ([Int],[Int],[Int],[Int]) -> Int -> [[[Int]]]
getSolution originalClues  n = 
    filter (isSolution originalClues (n-1) ) (findAllBoards n)
    




-- Given an n size NxN board give all permuations of board without repeats in rows, and no repeated rows in boards.
-- returns Array of 2D arrays
-- findAllBoards :: Int -> IO [[[Int]]]
findAllBoards :: Int -> [[[Int]]]
findAllBoards n = 
    choose n (permutations [1..n])

-- rows <- return (permutations [1..n])
--     boards <- return (choose n rows)
--     return boards


-- choose n list returns all permuations of list with length n

-- concatMap :: (a -> [b]) -> [a] -> [b]
-- Map a function over a list and concatenate the results.

-- Chooseh creates list of lists length n containing each combination of elements 
-- chooseh [1,2,3,4] [] with n=2
-- [[2,1],[3,1],[4,1],[3,2],[4,2],[4,3]]

-- choose :: Int -> [t] -> [[[t]]] 
choose n list = 


    concatMap permutations (chooseh list [])
    where
        chooseh []     r = if length r == n then [r] else []
        chooseh (x:xs) r 
            | length r == n = [r]
            | otherwise     = chooseh xs (x:r)++ chooseh xs r










choosetest []     r n = if length r == n then [r] else []
choosetest (x:xs) r n 
    | length r == n = [r]
    | otherwise     = choosetest xs (x:r) n ++ choosetest xs r n


-- showtable xxs = mapM_ (showrow. zip maxlens) xxs
--   where
--     maxlens = map (show . (+ 1)) (foldr (max.length.show) 0 (transpose xxs))
--     showcell (maxl,c) = printf ("%" ++ ml ++ "s") $ show c
--     showrow xs = mapM_ showcell xs >> putStrLn "" 

-- Sample grid is
-- Clues: [1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]
-- Answer: [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]  

-- [ [0,1,3,2,2,0], [1,4,1,3,2,3], [3,3,2,1,4,2],  [2,3,2,1,4,1], [2,1,4,2,3,2], [0,2,2,1,3,0]  ]


















