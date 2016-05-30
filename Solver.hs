module Solver (Piramidy (Piramidy), solve) where
import Control.Monad
import Data.List
import Data.Map as Map
import Data.Maybe

import Problem
import Elimination

-- Partial solution of the pyramid puzzle.
-- The data structure is optimized for fast lookup when validating
-- consecutive solutions.
data Partial =
  Partial
    Int  -- puzzle size
    PartialBoard -- board so far
    (Int, Int) -- coordinates for insertion of the next pyramid
    ([Maybe Int], [Maybe Int], [Maybe Int], [Maybe Int]) -- active edge constraints
  deriving Show
-- The partial board is represented as a list of rows and a map of columns.
-- When applying a step, we append a pyramid to a row and update a column.
type PartialBoard = ([[Int]], Map.Map Int [Int])

solve :: Piramidy -> Maybe [[Int]]
solve (Piramidy [] [] [] []) = Just []
solve problem@(Piramidy above below left right) =
  listToMaybe $ extractSolution <$> backtrack grow done reject apply empty
  where
    -- generate possible steps
    grow (Partial _ _ pos _) = options pos
    -- next coordinates
    advance (x, y) = let (dx, x') = (x + 1) `quotRem` n in (x', y + dx)
    -- is the solution complete?
    done (Partial n _ (x, y) _) = (x, y) == (0, n)
    -- should the step be rejected?
    reject partial@(Partial n board (x,y) (a:_, b:_, l:_, r:_)) value =
      -- pyramid has not yet been used in current row
      value `elem` row ||
      -- pyramid has not yet been used in current column
      value `elem` column ||
      -- column has the correct number of pyramids seen from both edges
      (if y == n-1
        then rejectByEdgeConstraint b (value:column) ||
             rejectByEdgeConstraint a (reverse $ value:column)
        else False) ||
      -- row has the correct number of pyramids seen from both edges
      (if x == n-1
        then rejectByEdgeConstraint r (value:row) ||
             rejectByEdgeConstraint l (reverse $ value:row)
        else False)

      where row = currentRow partial :: [Int]
            column = currentColumn partial :: [Int]
            rejectByEdgeConstraint e r = not $ lineConstraintHolds e r
    -- apply a step to a partial solution, creating a larger partial solution
    apply p@(Partial n board pos (_:as, _:bs, _:ls, _:rs)) value =
      Partial n (addToBoard p value) (advance pos) (as, bs, ls, rs)
    -- empty partial solution
    empty = Partial n (emptyBoard n) (0, 0)
                      (cycle above, cycle below,
                       cycle (left >>= (replicate n)),
                       cycle (right >>= (replicate n)))

    options = eliminate problem :: (Int, Int) -> [Int]
    n = length above

-- General scheme for backtracking.
backtrack :: (a -> [b]) -> (a -> Bool) -> (a -> b -> Bool) ->
               (a -> b -> a) -> a -> [a]
backtrack grow done reject apply partial = do
  step <- grow partial
  when (reject partial step) mzero
  let partial' = apply partial step
  if done partial'
    then return partial'
    else backtrack grow done reject apply partial'

-- accessors and modifiers for partial solutions
currentRow :: Partial -> [Int]
currentRow (Partial _ ([], _) _ _) = []
currentRow (Partial _ _ (0, _) _) = []
currentRow (Partial _ (row:_, _) _ _) = row

currentColumn :: Partial -> [Int]
currentColumn (Partial size (_, columns) (x, _) _) = columns Map.! x

addToBoard :: Partial -> Int -> PartialBoard
addToBoard (Partial size (rows, columns) (0, _) _) value =
  ([value]:rows, adjust (value:) 0 columns)
addToBoard (Partial size (row:rows, columns) (x, _) _) value =
  ((value:row):rows, adjust (value:) x columns)

emptyBoard n = ([], Map.fromList [(i, []) | i <- [0..n-1]])

extractSolution :: Partial -> [[Int]]
extractSolution (Partial size (rows, _) _ _) = reverse $ reverse <$> rows
