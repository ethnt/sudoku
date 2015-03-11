-- Sudoku


-- Grid is one big string coming in, where 0 is a blank
-- "913025478..."

-- 1. Replace each cell by a list of Choices (choices functon)
--   "913025478" => [9], [1], [3], [1..9], [2], [5], [4], [7], [8]
-- 2. Generate a complete list of all possible grids (expand function)
-- 3. Identify and return all of the valid grids (valid function)
--    no duplicates in each row, column, or subgrid

-- solve is a higher-order point free function composition of choices, expand, and grid
solve = filter valid . expand . choices


-- Data types

type Digit = Char
type Row = [Digit]
