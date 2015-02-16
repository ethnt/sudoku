# Sudoku

A Sudoku solver.

## Solver

### Data Types

- `Grid`: Represents a 9-by-9 Sudoku grid.
- `Block`: A 3-by-3 sub-block inside of a Sudoku grid. There are 9 inside of a `Grid.`
- `Row`: A 9 cell long row of a Sudoku grid. Inside of a `Grid`.
- `Columns`: A 9 cell long column of a Soduku grid. Inside of a `Grid`.
- `Position`: A single cell. Can be inside of a `Block`, `Row`, or `Column`.

### Implementation

```haskell
solve :: Grid -> Grid
solve grid =
  -- Replace every 0 in the grid with a random number between 0-9 (randomizeBlanks)
  -- Check to see if it is correct by checking each Block, Row, and Column. If there is a failure, immediately return false (checkSolution)
  -- If not, try again (solve)
```
