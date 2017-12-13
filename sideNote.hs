module SideNote where
import Data.List
import Debug.Trace (trace)
{-
clueIsOne cluesTuple lst =
 case (fst cluesTuple) of
  1 -> eliminateWithSide (snd cluesTuple) (lst !! 0)
  2 -> eliminateWithSide (snd cluesTuple) (lst !! 1)
  3 -> eliminateWithSide (snd cluesTuple) (lst !! 2)
  4 -> eliminateWithSide (snd cluesTuple) (lst !! 3) 


eliminateWithSide :: [Char] -> [[[Int]]] -> Int -> [[[Int]]]
eliminateWithSide clueSide lst n clueToDelete =
 case clueSide of
 	"West"    -> 
 	"East"    ->
 	"Both"    ->
 	"Neither" ->




 	eliminateAll :: (Int,[Char]) -> [[[Int]]] -> Int -> [[Int]]
	eliminateAll _ [] _ = []
	eliminateAll cluesTuple lst clueToDelete =
	 case (fst cluesTuple) of
  1 -> deleteAll clueToDelete (lst !! 0) 
  2 -> deleteAll clueToDelete (lst !! 1) 
  3 -> deleteAll clueToDelete (lst !! 2) 
  4 -> deleteAll clueToDelete (lst !! 3)  

-}


{-
	special helper for clue = 1
	n == size of the puzzle i.e) 4
	clueIsOne3 (1,"East") [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ] 4
	clueIsOne3 (1,"West") [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ] 4
	clueIsOne3 (3,"East") [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ] 4
	clueIsOne3 (3,"West") [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ] 4
-}
clueIsOne3 :: (Int,[Char]) -> [[[Int]]] -> Int -> [[[Int]]]
-- this rn returns for the one Row * Maybe make it return [[[Int]]] as well?
clueIsOne3 cluesTuple lst n =
 case (snd cluesTuple) of
  "East" -> 
   let slices = (splitAt (fst cluesTuple) lst) in
    (delete (last (fst slices)) (fst slices)) ++ 
    [(insert [n]  (delete [1..(n-1)] ((eliminateAll cluesTuple lst n) !! (fst cluesTuple - 1))))] ++
    (snd slices)
 -- insert (insert [n]  (delete [1..(n-1)] ((eliminateAll cluesTuple lst n) !! (fst cluesTuple - 1)))) (delete (lst !! (fst cluesTuple)) lst)
  "West" -> 
   let slices = (splitAt (fst cluesTuple) lst) in
    (delete (last (fst slices)) (fst slices)) ++ 
    [(reverse (insert [n]  (delete [1..(n-1)] ((eliminateAll cluesTuple lst n) !! (fst cluesTuple - 1)))))] ++
    (snd slices)
-- reverse (insert (insert [n]  (delete [1..(n-1)] ((eliminateAll cluesTuple lst n) !! (fst cluesTuple - 1)))) (delete (lst !! (fst cluesTuple)) lst))
  "Both"    -> lst -- can't happend since the clue is One
  "Neither" -> lst -- no mutation on the board


{-
	special helper for clue = 4
	n == size of the puzzle i.e) 4
	clueIsN3 (1,"East") [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ] 4
	clueIsN3 (1,"West") [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ] 4
	clueIsN3 (3,"East") [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ] 4
	clueIsN3 (3,"West") [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ] 4

-}
clueIsN3 :: (Int,[Char]) -> [[[Int]]] -> Int -> [[[Int]]]
-- this rn returns for the one Row * Maybe make it return [[[Int]]] as well?
clueIsN3 cluesTuple lst n =
 case (snd cluesTuple) of
  "East" -> 
   let slices = (splitAt (fst cluesTuple) lst) in
    (delete (last (fst slices)) (fst slices)) ++ [(reverse ([x `insert` [] | x <- [1..n]]))] ++ (snd slices)
  "West" -> 
   let slices = (splitAt (fst cluesTuple) lst) in
    (delete (last (fst slices)) (fst slices)) ++ [[x `insert` [] | x <- [1..n]]] ++ (snd slices)
  "Both"    -> lst -- can't happend since the clue is N
  "Neither" -> lst -- no mutation on the board

-- [ [ [1,2,3,4] , [5,6,7,8], [1,2,3,4], [5,6,7,8] ],  ]

{-
	eleimObv (now takes care of columns too)
	ex) eliminateObviousOnes ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1])  [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ] 4
-}
eliminateObviousOnes originalClues board n =
    transpose (eliminateObviousRowOnes (getColClues originalClues n) (transpose (eliminateObviousRowOnes (getRowClues originalClues n) board n)) n)
-- eliminateObviousRowOnes (getRowClues ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1])) [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ] 4
-- transpose (eliminateObviousRowOnes (getRowClues ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1])) [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ] 4)
-- (eliminateObviousRowOnes (getColClues ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1])) (transpose (eliminateObviousRowOnes (getRowClues ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1])) [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ] 4)) 4)

{-
    Takes in the Original clues and array of array of possible numbers for the cell
    Then eliminates some of them according to the rule.
    Maybe for clues 1 and N for now.
    ([Int],[Int],[Int],[Int]) -> [[[Int]]] -> [[[Int]]]
    1) Get the clues from the orginial clues
    2) Note where 1 and N is
    3) elminate all 1s and Ns from that row/col

    ex) transpose (eliminateObviousRowOnes [[1,3],[3,1],[2,2],[2,2]] [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ] 4) == col
    eliminateObviousRowOnes [[1,4],[2,2],[3,1],[3,2]] [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ] 4
-}
-- eliminateObviousRowOnes :: ([Int],[Int],[Int],[Int]) -> [[[Int]]] -> Int -> [[[Int]]]
eliminateObviousRowOnes :: [[Int]] -> [[[Int]]] -> Int -> [[[Int]]]
eliminateObviousRowOnes _ [] _ = []
eliminateObviousRowOnes clues board n =
 reduceBoardRowClueN reducedRowClueOne cluesForN n 
  where 
   reducedRowClueOne :: [[[Int]]]
   reducedRowClueOne = reduceBoardRowClueOne board (whereIsTheClue clues 1) n
   cluesForN :: [(Int,[Char])]
   cluesForN = whereIsTheClue clues n
   -- whereIsTheClue (getClues ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) 4 4) 4
   -- whereIsTheClue (getClues ([1,2,3,3],[4,2,1,2],[2,1,2,4],[3,3,2,1]) 4 4) 4

-- eliminateObviousOnes originalClues lst n =
--  whereIsTheClue (getRowClues originalClues) 1
--  whereIsTheClue (getRowClues originalClues) 4
-- could use iterate since it takes the previous iteration's result Then take the last, but runtime...
-- iterate (\ clueIsOne clue(n) x n <- x == clueIsN clue(n) lst n)


{-
	recursive function that will mutate the board with list of clues for clue One
	takes in [[[Int]]] which represents the board
		     [(Int,[Char])] which represents the list of clues
		     n which is the size of the board
    ex) reduceBoardRowClueOne [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ]  [(1,"West"),(2,"Neither"),(3,"East"),(4,"Neither")] 4

-}
reduceBoardRowClueOne :: [[[Int]]] -> [(Int, [Char])] -> Int -> [[[Int]]]
reduceBoardRowClueOne board [] _ = board
reduceBoardRowClueOne board clues n =
 reduceBoardRowClueOne (clueIsOne3 (head clues) board n) (tail clues) n


{-
	recursive function that will mutate the board with list of clues for clue N
	takes in [[[Int]]] which represents the board
		     [(Int,[Char])] which represents the list of clues
		     n which is the size of the board
    ex) reduceBoardRowClueN [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ]  [(1,"East"),(2,"Neither"),(3,"Neither"),(4,"Neither")] 4
-}
reduceBoardRowClueN :: [[[Int]]] -> [(Int, [Char])] -> Int -> [[[Int]]]
reduceBoardRowClueN board [] _ = board
reduceBoardRowClueN board clues n =
 reduceBoardRowClueN (clueIsN3 (head clues) board n) (tail clues) n







{-
	eliminateN
	ex) eliminateN [(1,"West"),(2,"East")] [[[1,2],[1,2]],[[1,2],[1,2]]] 2
	ex) eliminateN [(1,"West"),(0,"Neither"),(3,"East"),(0,"Neither")] [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ] 4
-}
eliminateN :: [(Int,[Char])] -> [[[Int]]] -> Int -> [[[Int]]]
eliminateN [] _ _ = []
eliminateN _ [] _ = []
eliminateN clues lst n =
 if debugTrace ("line 118: " ++ show lst ++ " is it 0? ") (fst (head clues) /= 0)
  then debugTrace ("line 119: " ++ show lst ++ " body: ") ([(clueIsOne2 (head clues) lst n)] ++ (eliminateN (tail clues) (tail lst) n))
  else debugTrace ("line 120: " ++ show lst ++ " head: " ++ show [(head lst)] ++ "body:") ([(head lst)] ++ (eliminateN (tail clues) (tail lst) n))


-- returning [[Int]] version
{-
special helper for clue = 4
n == size of the puzzle i.e) 4
clueIsN2 (1,"East") [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ] 4
clueIsN2 (1,"West") [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ] 4
-}
clueIsN2 :: (Int,[Char]) -> [[[Int]]] -> Int -> [[Int]]
-- this rn returns for the one Row * Maybe make it return [[[Int]]] as well?
clueIsN2 cluesTuple lst n =
 case (snd cluesTuple) of
 "East" -> debugTrace ("line 134: ") [insert x [] | x <- [1..n]]
 "West" -> debugTrace ("line 135: ") (reverse [insert x [] | x <- [1..n]])
--"Both"    -> lst -- can't happen but what if I just make this lst !! 0 and use it later.. (Think this approach isn't the best since index is not considered..?)
--"Neither" -> lst -- can't happen

debugTrace :: Show a => String -> a -> a
debugTrace s v = trace (s ++ show v) v

{-
special helper for clue = 1
n == size of the puzzle i.e) 4
clueIsOne2 (1,"East") [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ] 4
clueIsOne2 (1,"West") [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ] 4
-}
clueIsOne2 :: (Int,[Char]) -> [[[Int]]] -> Int -> [[Int]]
-- this rn returns for the one Row * Maybe make it return [[[Int]]] as well?
clueIsOne2 cluesTuple lst n =
 case (snd cluesTuple) of
 "East" -> insert [n]  (delete [1..(n-1)] ((debugTrace "indexed array E = " $ eliminateAll cluesTuple lst n) !! debugTrace "line 152: index = " (fst cluesTuple - 1)) )
 "West" -> reverse (insert [n]  (delete [1..(n-1)] ((debugTrace "indexed array W = " $ eliminateAll cluesTuple lst n) !! debugTrace "line 153: index = " (fst cluesTuple - 1))))
--"Both"    -> lst -- can't happen
--"Neither" -> lst -- can't happen






{-
	get rid of the N given the clue is one
	eliminateAll  (1,"West") [ [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]],[[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]] ] 1
-}

eliminateAll :: (Int,[Char]) -> [[[Int]]] -> Int -> [[[Int]]]
eliminateAll _ [] _ = []
eliminateAll cluesTuple lst numberToDelete =
 case (fst cluesTuple) of
  0 -> lst
  1 -> debugTrace "line 172: " ([(deleteAll numberToDelete (lst !! 0))] ++ [(lst !! 1)] ++ [(lst !! 2)] ++ [(lst !! 3)])
  2 -> debugTrace "line 173: " ([(lst !! 0)] ++ [(deleteAll numberToDelete (lst !! 1))] ++ [(lst !! 2)] ++ [(lst !! 3)])
  3 -> debugTrace "line 174: " ([(lst !! 0)] ++ [(lst !! 1)] ++ [(deleteAll numberToDelete (lst !! 2))] ++ [(lst !! 3)])
  4 -> debugTrace "line 175: " ([(lst !! 0)] ++ [(lst !! 1)] ++ [(lst !! 2)] ++ [(deleteAll numberToDelete (lst !! 3))])

 {-
	helper to delete all instance of a number within the input [[Int]]
	ex) deleteAll 1 [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]]
 -}
deleteAll :: Int -> [[Int]] -> [[Int]]
deleteAll _ [] = []
deleteAll clueToDelete lst =
 foldr (\ x y -> if clueToDelete `elem` x 
  then [(delete clueToDelete x)] ++ y 
   else [x] ++ y) [] lst



-- module CheckSolutions where


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
	n is n-1 here!
	ex) isSolution ([1,2,3,3],[4,2,1,2],[2,1,2,4],[3,3,2,1]) 3 [[4,3,2,1],[3,4,1,2],[2,1,3,4],[1,2,4,3]]
	ex) isSolution ([2,1,2,5,3], [3,2,1,3,2], [2,1,2,4,2], [2,2,3,1,2]) 4 [[2,5,4,1,3],[5,3,1,2,4],[1,4,2,3,5],[3,2,5,4,1],[4,1,3,5,2]]
 -}
isSolution :: ([Int],[Int],[Int],[Int]) -> Int -> [[Int]] -> Bool
isSolution (_,_,_,_) _ [] = False
-- isSolution (top,right,bottom,left) lst
isSolution originalClues n soln =
 isAllTrue (isValidRows soln (getRowClues originalClues (n+1)) n) &&
 isAllTrue (isValidRows (rowToColumns soln (n+1)) (getColClues originalClues (n+1)) n) -- Check Col's validity

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
 | n == 0 = True
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
	rowToColumns [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]] 4
	rowToColumns [[2,5,4,1,3],[5,3,1,2,4],[1,4,2,3,5],[3,2,5,4,1],[4,1,3,5,2]] 1
			-> [2,5,1,3,4]

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

	ex) getClues ([1,2,3,3],[4,2,1,2],[2,1,2,4],[3,3,2,1]) 4 4 -> [[1,4],[2,2],[3,1],[3,2]]
-}
getClues :: ([Int],[Int],[Int],[Int]) -> Int -> Int -> [[Int]]
getClues clues n m =
 [getClue clues n x | x <- [1.. m]]


{-
	getClue gets on row of a clue
	m is 1
	if n = 4 -> we get the clue for a row
	if n = 3 -> we get the clue for a column
	ex) getClue ([1,2,3,3],[4,2,1,2],[2,1,2,4],[3,3,2,1]) 4 1 -> [1,4]
    ex) getClue ([2,1,2,5,3], [3,2,1,3,2], [2,1,2,4,2], [2,2,3,1,2]) 4 5 -> [2,2]
    ex) getClue ([2,1,2,5,3], [3,2,1,3,2], [2,1,2,4,2], [2,2,3,1,2]) 4 1 -> [2,3]

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
	ex) getRowClues ([1,2,3,3],[4,2,1,2],[2,1,2,4],[3,3,2,1]) 4
	ex) getRowClues ([2,1,2,5,3], [3,2,1,3,2], [2,1,2,4,2], [2,2,3,1,2]) 5
 -}
getRowClues :: ([Int],[Int],[Int],[Int]) -> Int -> [[Int]]
getRowClues clues n = [getClue clues 4 x | x <- [1..n]]

 {-
	get the clues for columns
	ex) getColClues ([1,2,3,3],[4,2,1,2],[2,1,2,4],[3,3,2,1])
	ex) getColClues ([2,1,2,5,3], [3,2,1,3,2], [2,1,2,4,2], [2,2,3,1,2]) 5
 -}
getColClues :: ([Int],[Int],[Int],[Int]) -> Int -> [[Int]]
getColClues clues n = [reverse (getClue clues 3 x) | x <- [1..n]]





{-
	special helper for clue = 1
-}
-- clueIsOne :: [[[Int]]]



{-
	special helper for clue = N
-}
-- clueIsN




{-
	Takes in the clues for all rows and the clue to look up, and 
	return an array of Tuples (rowNumber, whichSide the clue is present)
	ex) whereIsTheClue [[1,4],[2,2],[3,1],[3,2]] 1
-}
whereIsTheClue :: [[Int]] -> Int -> [(Int,[Char])]
whereIsTheClue [] _ = []
whereIsTheClue cluesForRows clueToFind =
 zip (getIndices cluesForRows clueToFind 1 []) 
  [whichSideClue clues clueToFind | clues <- cluesForRows]
 

{-
	whichSideClue takes in [Int] which represents a clue for one row and Int which represents
	the clue to find, and return if it's in the West or East side of the clue.
-}
whichSideClue:: [Int] -> Int -> [Char]
whichSideClue [] _ = ""
whichSideClue clues clueToFind = 
 if (clues !! 0) == clueToFind && (clues !! 1) == clueToFind then "Both sides" 
  else (if (clues !! 0) == clueToFind then "West" 
   else (if (clues !! 1) == clueToFind then "East" else "Neither"))


{-
	takes in arrays of clues, clueToFind, Int and [Int] for 1-indexed counting and acc
	and return a list of Int which represents in which row the clueToFind is present.
	0 means the row does not have the clue.
	n = 1 fixed for 1-base count
    acc = [] to start from empty array
	ex) getIndices [[1,4],[2,2],[3,1],[3,2]] 1 1 []
-}
getIndices :: [[Int]] -> Int -> Int -> [Int] -> [Int]
getIndices [] _ _ _ = []
getIndices cluesForRows clueToFind n acc =
 [n] ++ getIndices (tail cluesForRows) clueToFind (n+1) acc

