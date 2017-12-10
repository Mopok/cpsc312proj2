
import Data.List
import Text.Printf
import Data.List
import CheckSolutions
import Text.Printf
import Data.Char



-- -- showAns :: [[Int]] 
printStrings :: [[String]] -> IO ()
printStrings  = sequence_ . flattenActions . strToPrint  

flattenActions :: [[IO ()]] -> [IO ()]
flattenActions actions = concat $ intersperse [(putStrLn "")]  actions

strToPrint :: [[String]] -> [[IO ()]]
strToPrint strs = map (map putStrLn) strs  

-- intToPrint :: Show a => [[a]] -> [[String]]
intToPrint ints = map intsToString ints

-- intToPrint [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
intsToString :: [Int] -> String
intsToString [] = []
intsToString (h:t) =
    (show h)++(intsToString t)


printAns ans = 
    putStr ( unlines  (map show ans) )
-- printAns [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]



-- Brute Force Solution

{-
   getSolution ([2,1],[1,2],[2,1],[1,2]) 2 
   getSolution ([2,2,1],[1,2,2],[3,1,2],[2,1,3]) 3
    getSolution ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) 4
    [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
   getSolution ([0,1,2,0],[0,0,0,2],[3,0,3,0],[0,0,0,0]) 4
    [[1,4,3,2],[2,3,1,4],[4,1,2,3],[3,2,4,1]]
   getSolution ([2,1,2,5,3], [3,2,1,3,2], [2,1,2,4,2], [2,2,3,1,2]) 5
    [[2,5,4,1,3],[5,3,1,2,4],[1,4,2,3,5],[3,2,5,4,1],[4,1,3,5,2]]

-}
getSolution :: ([Int], [Int], [Int], [Int]) -> Int -> [[Int]]
getSolution originalClues  n = 
    filterStop (isSolution originalClues (n-1) ) (findAllBoards n)
    

-- Given an n size NxN board give all permuations of board without repeats in rows, and no repeated rows in boards. ~ (n!)^n
-- returns Array of 2D arrays
-- findAllBoards :: Int -> IO [[[Int]]]
findAllBoards :: Int -> [[[Int]]]
findAllBoards n = 
    choose n (permutations [1..n])


-- choose n list returns all permuations of list with length n

-- concatMap :: (a -> [b]) -> [a] -> [b]
-- Map a function over a list and concatenate the results.

-- Chooseh creates list of lists length n containing each combination of elements 
-- chooseh [1,2,3,4] [] with n=2
-- [[2,1],[3,1],[4,1],[3,2],[4,2],[4,3]]

choose :: Int -> [a] -> [[a]]
choose n list = 
    concatMap permutations (chooseh list [])
    where
        chooseh []     r = if length r == n then [r] else []
        chooseh (x:xs) r 
            | length r == n = [r]
            | otherwise     = chooseh xs (x:r)++ chooseh xs r





-- showtable xxs = mapM_ (showrow. zip maxlens) xxs
--   where
--     maxlens = map (show . (+ 1)) (foldr (max.length.show) 0 (transpose xxs))
--     showcell (maxl,c) = printf ("%" ++ ml ++ "s") $ show c
--     showrow xs = mapM_ showcell xs >> putStrLn "" 

filterStop :: (p -> Bool) -> [p] -> p
filterStop fxn (x:xs) 
    | fxn x     = x
    | otherwise = filterStop fxn xs



-- Sample grid is
-- Clues: [1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]
-- Answer: [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]  

-- [ [0,1,3,2,2,0], [1,4,1,3,2,3], [3,3,2,1,4,2],  [2,3,2,1,4,1], [2,1,4,2,3,2], [0,2,2,1,3,0]  ]




-- Smart Solution:

-- List of n rows
-- Rows is a list of N cells
-- cell is a list of possible numbers that could be in that cell

-- Using absolute brute force with this method will cause ~ n^(n*n) permutations. 
-- However if we can reduce possibilities in each cell using solving techniques we can drastically reduce this number to where it is faster than the other brute force method.

-- Get reduced cell grid with solving techniques
-- flattenBoard
-- find all permutations

-- Brute Force 3x3 (3^9 permutations ~ 19k)  exmaple 
-- findSolution [[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]]] ([2,2,1],[1,2,2],[3,1,2],[2,1,3]) 3



findSolution lst originalClues n =
    filterStop (isSolution originalClues (n-1) ) (unFlattenBoards(allPermutations (flattenBoard lst) ) n)

-- Given list of n rows returns n*n list of cells
flattenBoard :: Foldable t => t [a] -> [a]
flattenBoard lst = foldr (\ x  y -> x++y ) [] lst

-- flattenBoard [[1,2],[3,4]]  = [1,2,3,4]
-- flattenBoard [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]     =     [4,1,3,2,2,3,4,1,3,2,1,4,1,4,2,3]
-- flattenBoard [[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]]]
-- = [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]]

-- Given n*n length list of cells gives list of possible flattened boards
-- Where each cell represents a tile on the grid, and is a list of possible numbers that could go there.
allPermutations :: [[Int]] -> [[Int]]
allPermutations (x:[]) = foldr (\ z y -> [z]:y ) [] x   -- Base case [[2,1]] -> [[2],[1]]
allPermutations ( cell :ys) = 
    allPermutationsh cell (allPermutations ys)

-- allPermutations [[1,2],[1,2],[1,2],[1,2]] = [[1,1,1,1],[1,1,1,2],[1,1,2,1],[1,1,2,2],[1,2,1,1],[1,2,1,2],[1,2,2,1],[1,2,2,2],[2,1,1,1],[2,1,1,2],[2,1,2,1],[2,1,2,2],[2,2,1,1],[2,2,1,2],[2,2,2,1],[2,2,2,2]]


-- Given one cells permutations and a list of existing permutations, create new board permutations
allPermutationsh :: [Int] -> [[Int]] -> [[Int]]
allPermutationsh [] lst = []
allPermutationsh (p:ps) lst = 
    (map (\ z -> p:z ) lst)++(allPermutationsh ps lst)

-- allPermutationsh [1,2] [[2],[1]] = [[2,1], [1,1], [2,2], [2,1]]

-- (map (\ z -> x:z ) ys)
-- append x[0..n] to each element making new list each time




-- NOT DONE
-- Generate permutations without repeat in rows and cols


-- Given n*n length list of cells and n, give permutations without repeats in rows and cols.
-- smartPermutations :: [[Int]] -> Int -> [[Int]]
-- smartPermutations (x:[]) _ = foldr (\ z y -> [z]:y ) [] x   -- Base case [[2,1]] -> [[2],[1]]
-- smartPermutations (cell :ys) n i =
--     (smartPermutationsh cell i (smartPermutations ys n (i+1) ) ) 


-- -- 


-- smartPermutationsh cell index permutations n = 



-- given cell value, index of cell, permutation to apply value, n board (ex 2,3,4), and board size (ex 4,9,16) 
-- Return board with value applied to permutation with emptys if not possible at any position
-- apply :: [Int] -> Int -> [[Int]] -> Int -> Int -> [Int]
-- apply cell i permutation n size =
--     applyRow




-- given list of boards and n (n*n) unflatten boards to have rows again. Returns List of Boards
unFlattenBoards :: [[Int]] -> Int -> [[[Int]]]
unFlattenBoards [] n = []
unFlattenBoards (x:xs) n = 
    (unFlattenBoard x n):(unFlattenBoards xs n)


-- Given flattened board and row size. (IE 2d board: [1,2,1,2]) Return unflattened board [[1,2],[1,2]]
unFlattenBoard :: [Int] -> Int -> [[Int]]
unFlattenBoard [] _ = []
unFlattenBoard lst n =
    (unFlattenBoardh (splitAt n lst) n)

unFlattenBoardh :: ([Int], [Int]) -> Int -> [[Int]]
unFlattenBoardh (x,[]) _ = [x]
unFlattenBoardh (x,y) n =
    x:(unFlattenBoardh (splitAt n y) n)

-- unFlattenBoard [1,2,1,2] 2 = [[1,2],[1,2]]


{-
    Takes in the Original clues and array of array of possible numbers for the cell
    Then eliminates some of them according to the rule.
    Maybe for clues 1 and N for now.
    ([Int],[Int],[Int],[Int]) -> [[[Int]]] -> [[[Int]]]
    1) Get the clues from the orginial clues
    2) Note where 1 and N is
    3) elminate all 1s and Ns from that row/col
-}
-- eliminateObviousOnes :: ([Int],[Int],[Int],[Int]) -> [[[Int]]] -> [[[Int]]]
-- eliminateObviousOnes _ [] = []
-- eliminateObviousOnes originalClues lst



eliminateObviousOnes originalClues lst =
    transpose (eliminateObviousRowOnes originalClues (transpose (eliminateObviousRowOnes originalClues board n) ) n )


