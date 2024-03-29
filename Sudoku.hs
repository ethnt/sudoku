module Sudoku where

import qualified Data.List (nub, elemIndex)
import qualified Data.Maybe (fromMaybe)

{-
    Sudoku.hs - solver for sudoku puzzles using an "elegant brute" approach

    Test grid is a list of lists of digit characters, with '0' meaning blank.

    Hints:
        - In functional programs, map is equivalent to the "for each" concept
        - Built-in functions exist for many common tasks - filter, all, etc.
-}
-- ******** Data Types ********

-- A Row is a list of values of any type
type Row a = [a]
-- A Matrix is a list of Rows of any type
type Matrix a = [Row a]
-- A Digit is a single character
type Digit = Char
-- A Grid is a matrix of digits
type Grid = Matrix Digit
-- We represent available choices as a list of digits
type Choices = [Digit]

-- ******** Top-Level Algorithm ********

{-
    Solve - brute force but elegant in the functional style with laziness
    1. Replace each blank cell by a list of all possible digits (choices)
    2. Generate a complete list of all possible grids given the choices (expand)
    3. Identify each valid grid (valid) and select out only such grids (filter)
    Example: head $ solveSuperSlow testPuzzle
-}
-- solve :: Grid -> Maybe Grid
-- solve grid = solveHelper (expand grid) Nothing
--         where solveHelper (g:gr) rsf | rsf /= Nothing = rsf
--                                      | gr == [] = Nothing
--                                      | otherwise = if (valid g) then solveHelper gr (Just g) else solveHelper gr Nothing

solve grid = filter valid (expand grid)


{-
    For each row... For each cell in that row...
        If it is blank, substitute in its place a list of all digits
        Else it's a digit, substitute a singleton list containing that digit
   Example: head $ choices testPuzzle =
                ["6","123456789","3","7","1","9","4","5","8"]  -- first row
-}
choices :: Grid -> Matrix Choices
choices grid = reverse $ choicesHelper grid []
        where choicesHelper [] rsf = rsf
              choicesHelper (g:gs) rsf = choicesHelper gs ((replaceBlanks (group 1 g)):rsf)
              replaceBlanks row = replaceBlanksHelper row []
                            where replaceBlanksHelper [] rsf = rsf
                                  replaceBlanksHelper (x:xs) rsf | x == "0" = replaceBlanksHelper xs (rsf ++ [digits])
                                                                 | otherwise = replaceBlanksHelper xs (rsf ++ [x])


{-
  Generate a complete list of all possible expansions of the grid given the
  choices for initially blank cells. Think of this as a... cartesian product!
  Note: Cells in the resulting grids will still be Choices, but each Choices
  list will have only one element at that point.
  Example: head $ expand $ choices testPuzzle = [ "613719458",
                                                  "718624931",
                                                  "491813276",
                                                  "569178342",
                                                  "837542169",
                                                  "141936587",
                                                  "314287195",
                                                  "975361814",
                                                  "286491713" ]

  Actual example: head $ expand testPuzzle = [ "613719458",
                                                "718624931",
                                                "491813276",
                                                "569178342",
                                                "837542169",
                                                "141936587",
                                                "314287195",
                                                "975361814",
                                                "286491713" ]
-}
expand :: Grid -> Matrix Choices
expand m = sequence $ map sequence $ choices m


{-
  A valid grid is one for which...
    each row contains no duplicates,
    each column contains no duplicates,
    each box (3x3) contains no duplicates
    Example: valid $ head $ expand testPuzzle = False
-}
valid :: Grid -> Bool
valid g = validCols g && validRows g && validBoxs g
    where validCols g = validColsHelper (cols g) True
              where validColsHelper _ False = False
                    validColsHelper (_:[]) True = False
                    validColsHelper (x:xs) rsf = validColsHelper xs (nodups x)

          validRows g = validRowsHelper (rows g) True
                  where validRowsHelper _ False = False
                        validRowsHelper (_:[]) True = False
                        validRowsHelper (x:xs) rsf = validRowsHelper xs (nodups x)

          validBoxs g = validBoxsHelper (boxs g) True
                  where validBoxsHelper _ False = False
                        validBoxsHelper (_:[]) True = False
                        validBoxsHelper (x:xs) rsf = validBoxsHelper xs (nodups x)



-- ******** Auxiliary Functions ********


-- Just a list of all the valid digits.
-- digits :: [a]
digits = ['1'..'9']


{-
  Simple predicate that tests if a cell is blank.
  Use point-free partial function application.
-}
blank :: Choices -> Bool
blank = elem '0'


{-
  Computes, as a list, the Cartesian product of lists. A Cartesian product of
  N sets is the set of N-tuples of values of each of the original sets.

  Example: cp [[1,2],[4,5]] = [[1,4],[1,5],[2,4],[2,5]]
  Tip: You can use a list comprehension for this.
-}
cp :: [[a]] -> [(a, a)]
cp lists = [ (x, y) | x <- xs, y <- ys ]
           where xs = head lists
                 ys = head (tail lists)


{-
  Grabs sub-lists of n elements at-a-time and builds a list of these groups.
  Example: group 3 "603719458" = ["603","719","458"]
-}
group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = ys : group n zs
  where (ys, zs) = splitAt n xs


{-
  Returns a list of "groups" to its original form.
  Example: ungroup ["603","719","458"] = "603719458"
  Tip: There is a pre-defined function that does exactly what we need.
-}
ungroup :: [[a]] -> [a]
ungroup xs = concat xs


{-
  Returns a list whose elements are rows of the original matrix.
  Tip: This one is trivial since a grid should already be a list of rows!
-}
rows :: Grid -> Grid
rows mtrx = mtrx


{-
  Returns a list whose elements are the columns from the original matrix.
  Tip: Apply your unparalleled mastery of recursion.
-}
cols :: Grid -> Grid
cols ([]:_) = []
cols mtrx = (map head mtrx) : cols (map tail mtrx)


{-
  Returns a list whose elements represent all of non-overlapping 3x3
  sub-grids from the original matrix, so that each sub-grid is a single
  list.
  Example: boxs testPuzzle = [ "603708491", ..., "095804713" ]
  Tip: Use point-free function compisition with only map, group, ungroup, cols
-}
boxs :: Grid -> Grid
boxs mtrx = concat $ map (group 9 . concat) (boxsHelper mtrx)
  where boxsHelper ([]:_) = []
        boxsHelper mtrx = group 9 $ concat $ map (head . group 3) mtrx : boxsHelper (map (concat . tail . group 3) mtrx)


{-
  Returns true provided no element of the list appears twice, false otherwise.
  Tip: Look up the pre-defined function all from Prelude.
-}
nodups :: (Eq a) => [a] -> Bool
nodups xs = (Data.List.nub xs) == xs -- To-do: Make own implementation


-- ***************************

testPuzzle = [
    "603719458",
    "708624931",
    "491803276",
    "569178342",
    "837542169",
    "140936587",
    "314287095",
    "975361804",
    "286490713"
    ]

testPuzzle2 = [
    "050060001",
    "004800070",
    "800000052",
    "200057030",
    "000000000",
    "030690005",
    "790000008",
    "010006500",
    "500030060"
    ]

main = print (head $ solve testPuzzle2)
