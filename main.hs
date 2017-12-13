module Main where
import SideNote
import Data.List
import Text.Printf
import Data.List
import Text.Printf
import Data.Char
import Data.Maybe

main :: IO ()
main = return ()


printAns :: Show a => [a] -> IO ()
printAns ans = 
    putStr ( unlines  (map show ans) )
-- printAns [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]


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

-- DEMOS!

-- getSolutionBruteForce ([2,2,1],[1,2,2],[3,1,2],[2,1,3]) 3
-- getSolutionBruteForce ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) 4
-- getSolutionBruteForce ([0,1,2,0],[0,0,0,2],[3,0,3,0],[0,0,0,0]) 4

-- Using a basic bruteForce technique with no repeats in rows only. Works well for 4x4 any larger it will not finish

getSolutionBruteForce originalClues  n = 
    printAns (filterStop (isSolution originalClues (n-1) ) (findAllBoards n) )

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
-- getSolutionBruteForce2 ([2,2,1],[1,2,2],[3,1,2],[2,1,3]) 3
-- Brute force, has repeats in rows and cols. Very slow
getSolutionBruteForce2 originalClues n =
    printAns (filterStop (isSolution originalClues (n-1) ) (unFlattenBoards(allPermutations (flattenBoard (createGridBoard n 0)) ) n) )

-- getSolutionBruteForce3 ([2,2,1],[1,2,2],[3,1,2],[2,1,3]) 3
-- getSolutionBruteForce3 ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) 4                   [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
-- getSolutionBruteForce3 ([0,1,2,0],[0,0,0,2],[3,0,3,0],[0,0,0,0]) 4                   [[1,4,3,2],[2,3,1,4],[4,1,2,3],[3,2,4,1]]
-- getSolutionBruteForce3 ([2,1,2,5,3], [3,2,1,3,2], [2,1,2,4,2], [2,2,3,1,2]) 5        [[2,5,4,1,3],[5,3,1,2,4],[1,4,2,3,5],[3,2,5,4,1],[4,1,3,5,2]]
-- Generates smart permutations with no row and col repeats. Can also be reduced by clues.
getSolutionBruteForce3 originalClues n =
    printAns (filterStop (isSolution originalClues (n-1) ) (smartPermutations (createGridBoard n 0) [] n) )



-- getSolutionSmart1 ([2,2,1],[1,2,2],[3,1,2],[2,1,3]) 3
-- getSolutionSmart1 ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) 4                   [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
-- getSolutionSmart1 ([0,1,2,0],[0,0,0,2],[3,0,3,0],[0,0,0,0]) 4                   [[1,4,3,2],[2,3,1,4],[4,1,2,3],[3,2,4,1]]
-- getSolutionSmart1 ([2,1,2,5,3], [3,2,1,3,2], [2,1,2,4,2], [2,2,3,1,2]) 5        [[2,5,4,1,3],[5,3,1,2,4],[1,4,2,3,5],[3,2,5,4,1],[4,1,3,5,2]]
-- getSolutionSmart1 ([0,0,0,3,0],[0,4,2,0,3],[0,0,0,0,4],[3,0,0,0,4]) 5        [[2,3,4,1,5],[5,4,2,3,1],[4,1,3,5,2],[3,5,1,2,4],[1,2,5,4,3]]


-- 
getSolutionSmart1 originalClues n =
    printAns (filterStop (isSolution originalClues (n-1) )  (smartPermutations (solveUsingNandOne (createGridBoard n 0) originalClues n) [] n) )


-- To improve we can implement more solving strategies to speed it up. This will get more complex, however with this model we have the flexibility to do it.


-- smartPermutations (solveUsingNandOne (createGridBoard 4 0)  ([0,1,2,0],[0,0,0,2],[3,0,3,0],[0,0,0,0]) 4) [] 4
-- smartPermutations (solveUsingNandOne (createGridBoard 4 0)  ([0,1,2,0],[0,0,0,2],[3,0,3,0],[0,0,0,0]) 4) [] 4
-- smartPermutations (solveUsingNandOne (createGridBoard 5 0)  ([2,1,2,5,3], [3,2,1,3,2], [2,1,2,4,2], [2,2,3,1,2]) 5) [] 5


-- given n 0, create a fully populated board like so: [[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]]]
createGridBoard :: (Ord t, Num t, Enum t) => t -> t -> [[[t]]]
createGridBoard n i 
    | i < n     = (createGridRow n 0):(createGridBoard n (i+1) )
    | otherwise = []
    where
        createGridRow n i 
            | i < n      = (createGridCell n):(createGridRow n (i+1) )
            | otherwise  = []
            where
                createGridCell n = [1..n]


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





-- Generate permutations without repeat in rows and cols (No repeats in rows and cols.)
-- 2nd attempt at smart permutations. Uses maybe to disregard unfinshable board permutations. 
-- 2d grid of cells, and board accumulator returns array of 2d grids
smartPermutationsH :: Maybe [[[Int]]] -> [Int] -> Int -> [[[Int]]]
smartPermutationsH rest accm n
    | (isNothing rest)  = []
    | otherwise         = smartPermutations (fromJust rest) accm n

smartPermutations :: [[[Int]]] -> [Int] -> Int -> [[[Int]]]
smartPermutations rest accm n
    | (length rest) == 0            = [(unFlattenBoard accm n)]
    | (length ((rest!!0)!!0)) == 1  = smartPermutationsH (removeDupsAndFirstCell rest)  (fillAccm accm (((rest!!0)!!0)!!0) ) n
    | otherwise                     = (smartPermutationsH (removeDupsAndFirstCell rest)  (fillAccm accm (((rest!!0)!!0)!!0) ) n)++
                                        (smartPermutationsH (removeFirstElem rest) accm n)


-- smartPermutations [[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]]] [] 3
-- smartPermutations [[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]]] [] 4
-- smartPermutations (createGridBoard 4 0) [] 4
-- smartPermutations (createGridBoard 5 0) [] 5 WORKs! just takes like 1-2mins

    -- first call has chosen an option for position (0,0), and will move on to next cell. 
-- (([[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]]]!!0)!!0)!!0
-- length ((rest!!0)!!0) gives number of options for a cell
-- ((rest!!0)!!0)!!0) gives first cell value.

-- Adds element to accumulator
fillAccm :: [a] -> a -> [a]
fillAccm accm x = accm++[x]

-- Gets first elem from first cell. Removes first cell. Removes first elem from same cols and rows.
-- Checks if the board has any empty cells (meaning no possible board can be made.) returns nothing if that happens

removeDupsAndFirstCell :: [[[Int]]] -> Maybe [[[Int]]]
removeDupsAndFirstCell board =
    checkForEmptyCells (removeFirstCell (cleanBoard board (((board!!0)!!0)!!0) ) )
-- removeDupsAndFirstCell [[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]]]
-- removeDupsAndFirstCell [[[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]]]
-- removeDupsAndFirstCell [[[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]]]
-- removeDupsAndFirstCell [[[1,2,3]],[[1],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]]]
-- removeDupsAndFirstCell [[[1,2,3],[1],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]]]



checkForEmptyCells :: Foldable t => [[t a]] -> Maybe [[t a]]
checkForEmptyCells board 
    | isJust (checkForEmptyCellsH board) = Just board
    | otherwise = Nothing

-- checkForEmptyCells [[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]]]
-- checkForEmptyCells [[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]],[[],[1,2,3],[1,2,3]]]
-- checkForEmptyCells [[[],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]]]

checkForEmptyCellsH :: Foldable t => [[t a]] -> Maybe Bool
checkForEmptyCellsH [] = Just True
checkForEmptyCellsH (row:rest) 
    | isJust (checkForEmptyCellsH2 row) = (checkForEmptyCellsH rest)
    | otherwise                        = Nothing

checkForEmptyCellsH2 :: Foldable t => [t a] -> Maybe Bool
checkForEmptyCellsH2 [] = Just True
checkForEmptyCellsH2 (cell:row) 
    | (length cell) > 0 = (checkForEmptyCellsH2 row)
    | otherwise   = Nothing

-- checkForEmptyCellsH [[1,2,3],[1,2,3],[1,2,3]]
-- checkForEmptyCellsH [[],[1,2,3],[1,2,3]]
-- checkForEmptyCellsH [[]]

-- cleanBoard removes repeats in rows and columns of the first element in the first cell in the given board.
-- Boards have their first element cell removed through recursion, so the first row is incomplete and the second row is always complete if it exists.
cleanBoard :: Eq t => [[[t]]] -> t -> [[[t]]]
cleanBoard (row:[]) x = (cleanSingleRow row x):[]
cleanBoard (row:rest) x =
    (cleanSingleRow row x):(cleanCol rest ((length (rest!!0)) - (length row)) x )
-- cleanBoard [[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]]] 1
-- cleanBoard [[[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]]] 2
-- cleanBoard [[[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]]] 3
-- cleanBoard [[[1,2,3],[1,2,3],[1,2,3]]] 3
-- (length (rest!!0)) - (length row) gives the column index of the cell being removed

-- j is base 1 index for row (starts at 1)
-- rowj is the row base 1 that wants to be cleaned
-- x is the value that we want to clean from rowj
cleanRow :: (Num a, Eq a, Eq t) => [[[t]]] -> a -> a -> t -> [[[t]]]
cleanRow [] _ _ _ = []
cleanRow (row:rest) rowj j x
    | j /= rowj    = row:(cleanRow rest rowj (j+1) x )
    | j == rowj    = (cleanSingleRow row x):rest
-- cleanRow (createGridBoard 4 0) 2 1 3

-- Removes x from each cell in the given row
cleanSingleRow :: Eq t => [[t]] -> t -> [[t]]
cleanSingleRow [] _ = []
cleanSingleRow (cell:rest) x =
    (filter (\ y -> y /= x) cell):(cleanSingleRow rest x)
-- cleanSingleRow [[1,2,3,4],[3,4]] 1

-- base 0
-- Removes x from each cell in the given col in the given board
cleanCol:: (Eq t1, Eq t2, Num t2) => [[[t1]]] -> t2 -> t1 -> [[[t1]]]
cleanCol [] _ _ = []
cleanCol (row:rest) colI x =
    (cleanColH row 0 colI x):(cleanCol rest colI x)
    where
        cleanColH [] _ _ _ = []
        cleanColH (cell:row) i colI x
            | i /= colI = cell:(cleanColH row (i+1) colI x)
            | i == colI = (filter (\ y -> y /= x) cell):row

-- cleanCol [[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]]] 2 1

-- col is Int representing column to change base 1
-- row is Int representing row to change base 1
-- i is incremntor meant to keep track of column to change (start at 1)
-- j is incremntor meant to keep track of row to change (start at 1)
-- x is the value to set (col,row) to
setCell :: (Num a1, Num a2, Ord a1, Ord a2) => [[[a3]]] -> a2 -> a1 -> a1 -> a3 -> [[[a3]]]
setCell (rowj:rest) col row j x 
    | j < row  = rowj:(setCell rest col row (j+1) x)
    | j == row = (setCellH rowj col 1 x):rest
-- setCell [[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]],[[1,2,3],[1,2,3],[1,2,3]]] 2 2 1 2
setCellH :: (Ord a1, Num a1) => [[a2]] -> a1 -> a1 -> a2 -> [[a2]]
setCellH (cell:rest) col i x
    | i < col  = cell:(setCellH rest col (i+1) x)
    | i == col = [x]:rest
-- setCellH [[1,2,3],[1,2,3],[1,2,3]] 3 1 1
-- setCellH [[1,2,3],[1,2,3],[1,2,3]] 2 1 2
-- setCellH [[1,2,3],[1,2,3],[1,2,3]] 1 1 1


-- Removes first cell from board, if row is empty, removes the row too.
removeFirstCell :: [[[Int]]] -> [[[Int]]]
removeFirstCell (row:rest) 
    | (length row) == 1 = rest
    | otherwise         = (removeFirstCellH row):rest
    where
        removeFirstCellH (cell:rest) = rest
-- removeFirstCell [[[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]]]
-- removeFirstCell [[[1,2,3,4]]]


-- removes first possiblity from first cell 
removeFirstElem :: [[[Int]]] -> Maybe [[[Int]]]
removeFirstElem (x:rest) = Just ((removeFirstElemH x):rest)
    where
        removeFirstElemH (y:rest) = (removeFirstElemH2 y):rest 
            where
                removeFirstElemH2 (z:rest) = rest
-- removeFirstElem [[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]]]

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




-- SIMPLIFY BOARD BY USING CLUES. 
-- Checks for 1's and n's


solveUsingNandOne :: [[[Int]]] -> ([Int], [Int], [Int], [Int]) -> Int -> [[[Int]]]
solveUsingNandOne board clues n =
    solveColsNandOne (solveRowsNandOne board (getRowClues clues n) n 1) (getColClues clues n) n 1
-- solveUsingNandOne (createGridBoard 5 0) ([2,1,2,5,3], [3,2,1,3,2], [2,1,2,4,2], [2,2,3,1,2]) 5
-- solveUsingNandOne (createGridBoard 4 0) ([1,2,3,3],[4,2,1,2],[2,1,2,4],[3,3,2,1]) 4

-- 1st argument is my board
-- 2nd argument is my colClues
-- 3rd argument is n
-- 4th argument is i is my col index base 1 (starts at 1 always)
solveColsNandOne :: (Num a, Ord a) => [[[a]]] -> [[a]] -> a -> a -> [[[a]]]
solveColsNandOne board [] _ _ = board
solveColsNandOne board (clue:rest) n j
    | j > n          = board
    | (clue!!0) == n = solveColsNandOne (solveUsingNTop board 1 j n) rest n (j+1)                         -- TopSide clue, so row adjacent to N clue is 1
    | (clue!!1) == n = solveColsNandOne (solveUsingNBot board n j n) rest n (j+1)                        -- botSide clue, so row adjacent to N clue is N
    | (clue!!0) == 1 = solveColsNandOne (solveUsingOneVert board 1 j n) rest n (j+1)                     -- TopSide clue, so row adjacent to 1 clue is 1
    | (clue!!1) == 1 = solveColsNandOne (solveUsingOneVert board n j n) rest n (j+1)                     -- botSide clue, so row adjacent to 1 clue is N
    | otherwise      = solveColsNandOne board rest n (j+1)

-- solveColsNandOne (createGridBoard 4 0) (getColClues ([1,2,3,3],[4,2,1,2],[2,1,2,4],[3,3,2,1]) 4) 4 1
-- solveColsNandOne (createGridBoard 5 0) (getColClues ([2,1,2,5,3], [3,2,1,3,2], [2,1,2,4,2], [2,2,3,1,2]) 5) 5 1

solveUsingOneVert :: (Eq a3, Num a1, Num a2, Ord a1, Ord a2) => [[[a3]]] -> a1 -> a2 -> a3 -> [[[a3]]]
solveUsingOneVert board row col n =
    setCell (cleanCol (cleanRow board row 1 n ) (col-1) n ) col row 1 n
-- solveUsingOneVert (createGridBoard 4 0) 1 2 4
-- solveUsingOneVert (createGridBoard 4 0) 4 2 4

-- Clean row 1 of N, 2 of N-1 etc..
-- Then set col of interest to 4,3,2,1 (N,N-1,N-2....)
-- row is the row adjacent to the N clue (base 1) (starts at N always)
-- col is the col of the N clue
-- n is from nxn board
solveUsingNBot :: (Ord t1, Ord t2, Num t1, Num t2) => [[[t2]]] -> p -> t1 -> t2 -> [[[t2]]]
solveUsingNBot board row col n =
    solveUsingNBotColH (solveUsingNBotRowH board n n) col n n
    where
        -- set col of interest to 4,3,2,1 (N,N-1,N-2....)
        -- board
        -- col to change
        -- index of row to change (base 1)(starts at N)
        -- n for nxn board
        solveUsingNBotColH board col i n
            | i > 0     = solveUsingNBotColH (setCell board col i 1 (n-i+1)) col (i-1) n
            | otherwise = board
        -- solveUsingNBotColH (createGridBoard 4 0) 1 4 4
        -- solveUsingNBotColH (createGridBoard 4 0) 3 4 4

        -- Clean row 1 of N, 2 of N-1 etc..
        -- j is my row counter (base 1) (starts at N)
        solveUsingNBotRowH board j n
            | j > 0     = solveUsingNBotRowH (cleanRow board (n-j+1) 1 j) (j-1) n
            | otherwise = board
        -- solveUsingNBotRowH (createGridBoard 4 0) 4 4
-- solveUsingNBot (createGridBoard 4 0) 4 1 4
-- solveUsingNBot (createGridBoard 4 0) 4 2 4


-- Clean row 1 of 1, row 2 of 2... row n of n
-- Then set col of interest to 1,2,3,4
-- row is the row adjacent to the N clue (base 1) (starts as 1 always)
-- col is the col of the N clue
 -- n from nxn board
solveUsingNTop :: (Ord t1, Ord t2, Num t1, Num t2) => [[[t2]]] -> p -> t1 -> t2 -> [[[t2]]]
solveUsingNTop board row col n =
    solveUsingNTopColH (solveUsingNTopRowH board 1 n) col 1 n
    where
        -- sets given col to 1,2..n
        -- board
        -- col to change
        -- index of row to change (base 1)(starts at 1)
        -- n for nxn board
        solveUsingNTopColH board col i n
            | i <= n        = (solveUsingNTopColH (setCell board col i 1 i) col (i+1) n)
            |otherwise      = board
        -- solveUsingNTopColH (createGridBoard 4 0) 1 1 4

        -- Cleans row 1 of 1, row 2 of 2... row n of n
        -- j is my row counter (base 1) (starts at 1)
        solveUsingNTopRowH board j n
            | j <= n    = solveUsingNTopRowH (cleanRow board j 1 j) (j+1) n
            | otherwise = board
        -- solveUsingNTopRowH (createGridBoard 4 0) 1 4

-- solveUsingNTop (createGridBoard 4 0) 1 4 4
-- solveUsingNTop (createGridBoard 4 0) 1 3 4



-- 1st argument is my board
-- 2nd argument is my rowClues
-- 3rd argument is n
-- 4th argument is j is my row index base 1 (starts at 1 always)
solveRowsNandOne :: (Num a, Ord a) => [[[a]]] -> [[a]] -> a -> a -> [[[a]]]
solveRowsNandOne board [] _ _ = board
solveRowsNandOne board (clue:rest) n j
    | j > n          = board
    | (clue!!0) == n = solveRowsNandOne (solveUsingNLeft board 1 j n) rest n (j+1)                         -- leftSide clue, so column adjacent to N clue is 1
    | (clue!!1) == n = solveRowsNandOne (solveUsingNRight board n j n) rest n (j+1)                        -- rightSide clue, so column adjacent to N clue is N
    | (clue!!0) == 1 = solveRowsNandOne (solveUsingOneHorz board 1 j n) rest n (j+1)                     -- leftSide clue, so column adjacent to 1 clue is 1
    | (clue!!1) == 1 = solveRowsNandOne (solveUsingOneHorz board n j n) rest n (j+1)                     -- rightSide clue, so column adjacent to 1 clue is N
    | otherwise      = solveRowsNandOne board rest n (j+1)

-- solveRowsNandOne (createGridBoard 4 0) (getRowClues ([1,2,3,3],[4,2,1,2],[2,1,2,4],[3,3,2,1]) 4) 4 1
-- solveRowsNandOne (createGridBoard 5 0) (getRowClues ([2,1,2,5,3], [3,2,1,3,2], [2,1,2,4,2], [2,2,3,1,2]) 5) 5 1

-- Set adjacent cell to clue to N and clean that col of N and clean that row of N
-- col is always 1(for Left) or n(for Right)
-- row is base 1 Int representing what row the 1 clue was in
-- returns a board
solveUsingOneHorz :: (Eq a3, Num a1, Num a2, Ord a1, Ord a2) => [[[a3]]] -> a2 -> a1 -> a3 -> [[[a3]]]
solveUsingOneHorz board col row n =
    setCell (cleanRow (cleanCol board (col-1) n) row 1 n) col row 1 n
-- solveUsingOneHorz (createGridBoard 4 0) 1 3 4
-- solveUsingOneHorz (createGridBoard 4 0) 4 3 4

-- returns a board
-- col is Int representing column adjacent to N clue (always n)
-- row is Int representing row with said clue (1 to n)
solveUsingNRight :: (Ord t1, Ord t2, Num t1, Num t2) => [[[t2]]] -> p -> t1 -> t2 -> [[[t2]]]
solveUsingNRight board col row n =
    solveUsingNRightRowH (solveUsingNRightColH board 1 n) row n n
    where
        -- removes 1 from col n, 2 from col n-1, etc..
        -- i is index for column (base 1 ) (starts at 1)
        solveUsingNRightColH board i n
            | i <= n    = solveUsingNRightColH (cleanCol board (i-1) (n-(i-1)) ) (i+1) n
            | otherwise = board
        -- solveUsingNRightColH (createGridBoard 4 0) 1 4

        -- sets row to N, N-1, N-2 ... 1
        -- base 1 j, keeps track of column were on. (starts at n)
        -- row is the row you wanna set (base 1)
        -- n is n for nxn board
        solveUsingNRightRowH board row j n
            | j > 0     = solveUsingNRightRowH (setCell board j row 1 (n-j+1) ) row (j-1) n
            | otherwise = board
        -- solveUsingNRightRowH (createGridBoard 4 0) 1 4 4
        -- solveUsingNRightRowH (createGridBoard 4 0) 4 4 4

-- solveUsingNRight (createGridBoard 4 0) 4 3 4

-- returns a board
-- col is Int representing column adjacent to N clue (always 1)
-- row is Int representing row with said clue (1 to n)
solveUsingNLeft :: (Ord t1, Ord t2, Num t2, Num t1) => [[[t2]]] -> p -> t1 -> t2 -> [[[t2]]]
solveUsingNLeft board col row n =
    solveUsingNLeftRowH (solveUsingNLeftColH board 1 n) row 1 n
    where 
        -- removes 1 from col 1, 2 from col 2 etc..
        -- base 1 i, i keeps track of column were on. (starts at 1)
        solveUsingNLeftColH board i n
            | i <= n    = solveUsingNLeftColH (cleanCol board (i-1) i) (i+1) n
            | otherwise = board
        -- solveUsingNLeftColH (createGridBoard 4 0) 1 4

        -- sets row to 1,2..N
        -- base 1 j, keeps track of column were on. (starts at 1)
        -- row is the row you wanna set (base 1)
        -- n is n for nxn board
        solveUsingNLeftRowH board row j n
            | j <= n        = (solveUsingNLeftRowH (setCell board j row 1 j) row (j+1) n)
            | otherwise     = board
        -- solveUsingNLeftRowH (createGridBoard 4 0) 1 1 4
        -- solveUsingNLeftRowH (createGridBoard 4 0) 4 1 4

-- solveUsingNLeft (createGridBoard 4 0) 1 3 4




