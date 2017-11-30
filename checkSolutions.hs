module CheckSolutions where

-- CPSC 312 Project 2 Jin Min
-- import Math.Geometry.Grid.Square
-- zed :: ([Int],[Int],[Int],[Int]) -> [[Int]]

{-
	for the first tuple, where 1 is is where the result's n (4) is.
	
	
-}

{-
	checks if all the elments in the list are unique
-}
allUnique :: [Int] -> Bool
allUnique [] = True;
allUnique (h:t) = if (h `elem` t) then False else allUnique t


{-
	isValidRow takes in an array of Int which represents a row,
	two Int clue from both sides, in West then East order, 
	and return true if the row clues are satisfied.

	* two Int must be [1,n] where n is the length of the [Int]

	ex) isValidRow [4,3,2,1] 1 4 => True
		isValidRow [3,4,1,2] 2 2 => True
		isValidRow [4,1,2,3] 1 3 => False
		isValidRow [2,1,3,4] 3 2 => False
-}
isValidRow :: [Int] -> Int -> Int -> Bool
isValidRow [] _ _ = True
isValidRow lst w e = 
 ((ascendMatch lst w) && (ascendMatch (reverse lst) e)) && allUnique lst

-- by returning [Bool], we can see which row is not valid ; useful for user maybe?
{-
	n is (n-1)
	ex) [[4,3,2,1],[3,4,1,2],[2,1,3,4],[1,2,4,3]] [[1,4],[2,2],[3,1],[3,2]] 3
-}
isValidRows :: [[Int]] -> [[Int]] -> Int -> [Bool]
isValidRows [] _ _ = [False]
isValidRows _ [] _ = [False]
isValidRows soln clues n =
 [isValidRow (soln !! x) ((clues!!x)!!0) ((clues!!x)!!1) | x <- [0..n]] -- rn it's [Bool]


{-
	n is (n-1)
	ex) [[4,3,2,1],[3,4,1,2],[2,1,3,4],[1,2,4,3]] [[1,4],[2,2],[3,1],[3,2]] 3

	isValidCols :: [[Int]] -> [[Int]] -> Int -> [Bool]
	isValidCols [] _ _ = [False]
	isValidCols _ [] _ = [False]
	isValidCols soln clues n =
 	[isValidCols (soln !! x) ((clues!!x)!!0) ((clues!!x)!!1) | x <- [0..n]] -- rn it's [Bool]

-}

-- returns true if all elements are True
isAllTrue :: [Bool] -> Bool
isAllTrue [] = True;
isAllTrue (h:t) = if (h == True) then isAllTrue t else False

 {-
	isSolution takes in an array of [Int], return true if it's the correct solution to
	the puzzle.
	Maybe take in the west&east clue as well? ([Int],[Int],[Int],[Int])
	The four tuple is the source for the clue
	[Int] is Soln we want to check
	Int is the size n
	
	ex) isSolution ([1,2,3,3],[4,2,1,2],[2,1,2,4],[3,3,2,1]) [[4,3,2,1],[3,4,1,2],[2,1,3,4],[1,2,4,3]] 3
 -}
isSolution :: ([Int],[Int],[Int],[Int]) -> Int -> [[Int]] -> Bool
isSolution (_,_,_,_) _ [] = False
-- isSolution (top,right,bottom,left) lst
isSolution originalClues n soln =
 isAllTrue (isValidRows soln (getRowClues originalClues) n) &&
 isAllTrue (isValidRows soln (getColClues originalClues) n) 

-- isAllTrue (isValidRows soln (getColClues originalClues) n)
-- n = 3
-- originalClues = ([1,2,3,3],[4,2,1,2],[2,1,2,4],[3,3,2,1])
-- soln = [[4,3,2,1],[3,4,1,2],[2,1,3,4],[1,2,4,3]]

-- reverse the Array for the second Int (West side)
{-
	ascendMatch takes an array of Int and an Int, and return
	True if the number of elements in array that are in ascending order
	matches the Int

	* If the arry is empty... what to return?

	ex) ascendMatch [4,3,2,1] 1 => True
		ascendMatch [3,4,1,2] 2 => True
		ascendMatch [4,3,2,1] 4 => False
-}

-- need spec
countHelper :: [Int] -> Int -> Int -> Int -> Bool
countHelper [] _ x n = if x == n then True else False
countHelper lst max_so_far count n
 | (head lst) > max_so_far = countHelper (tail lst) (head lst) (count+1) n
 | otherwise = countHelper (tail lst) max_so_far count n

ascendMatch :: [Int] -> Int -> Bool
ascendMatch [] _ = True
ascendMatch lst n
 | n == 1 && (head lst) == (maximum lst) = True
 | otherwise = countHelper lst 0 0 n



{-
	rowToColumn takes in an array of Int arrays which represents the board,
	takes in an Int which represents which Column to convert into a row,
	and returns an array of Int which represents the converted column
	
	* Int must be less than the length of [Int]

	ex) rowToColumn [[4,3,2,1], [3,4,1,2], [2,1,3,4],[1,2,4,3]] 1 => [4,3,2,1]
		rowToColumn [[4,3,2,1], [3,4,1,2], [2,1,3,4],[1,2,4,3]] 2 => [3,4,1,2]
-}
rowToColumn :: [[Int]] -> Int -> [Int]
rowToColumn [] _ = []
rowToColumn lst n = 
 foldr (\ x y -> [(x !! (n-1))] ++ y) [] lst 

{-
	get all columns
	ex) rowToColumns [[4,3,2,1], [3,4,1,2], [2,1,3,4],[1,2,4,3]] 4

-}
rowToColumns :: [[Int]] -> Int -> [[Int]]
rowToColumns [] _ = []
rowToColumns lst n = [rowToColumn lst x | x <- [1..n]]

{-
	helper to get the clues from the tuple with four [Int]
	gets four of them first as columns could be done after transformation.

	parametrize the 4 and 0 as n and change it for the resursion
	
	n is either 3 or 4 to get a column / row clue
	m is [1..4]

	ex) getClues ([1,2,3,3],[4,2,1,2],[2,1,2,4],[3,3,2,1]) 4 -> [[1,4],[2,2],[3,1],[3,2]]
-}
getClues :: ([Int],[Int],[Int],[Int]) -> Int -> Int -> [[Int]]
getClues clues n m =
 [getClue clues n x | x <- [1.. m]]


{-
	getClue gets on row of a clue
	m is 1
	if n = 4 -> we get the clue for a row
	if n = 3 -> we get the clue for a column
	ex) getClue ([1,2,3,3],[4,2,1,2],[2,1,2,4],[3,3,2,1]) 4 1 ->

-}
getClue :: ([Int],[Int],[Int],[Int]) -> Int -> Int -> [Int]
getClue clues n m =
 [((getNth clues n) !! ((length (getNth clues n)) - m))] ++ [((getNth clues (n-2)) !! (m-1))]

{-
	helper to get nth element of n sized tuple
	is there way to generalize an arbitrary sized tuple?
-}
getNth :: ([Int],[Int],[Int],[Int]) -> Int -> [Int]
getNth (a,b,c,d) 1 = a
getNth (a,b,c,d) 2 = b
getNth (a,b,c,d) 3 = c
getNth (a,b,c,d) 4 = d


 {-
	get the clues for rows
 -}
getRowClues :: ([Int],[Int],[Int],[Int]) -> [[Int]]
getRowClues clues = [getClue clues 4 x | x <- [1..4]]

 {-
	get the clues for columns
 -}
getColClues :: ([Int],[Int],[Int],[Int]) -> [[Int]]
getColClues clues = [reverse (getClue clues 3 x) | x <- [1..4]]