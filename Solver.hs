module Solver (Piramidy (Piramidy), solve) where
import Control.Monad
import Data.List
import Data.Map as Map
import Data.Maybe

import Problem
import Elimination

type PartialBoard = ([[Int]], Map.Map Int [Int])
data Partial = Partial Int PartialBoard (Int, Int) deriving Show

solve :: Piramidy -> Maybe [[Int]]
solve (Piramidy [] [] [] []) = Just []
solve problem@(Piramidy above below left right) =
  listToMaybe $ extractSolution <$> backtrack grow done reject apply empty
  where
    grow (Partial n board pos) = options pos
    advance (x, y) = let (dx, x') = (x + 1) `quotRem` n in (x', y + dx)
    done (Partial n _ (x, y)) = (x, y) == (0, n)
    reject partial@(Partial n board (x,y)) value =
      value `elem` row ||
      value `elem` column ||
      (if y == n-1
        then rejectByEdgeConstraint (below !! x) (value:column) ||
             rejectByEdgeConstraint (above !! x) (reverse $ value:column)
        else False) ||
      (if x == n-1
        then rejectByEdgeConstraint (right !! y) (value:row) ||
             rejectByEdgeConstraint (left !! y) (reverse $ value:row)
        else False)

      where row = currentRow partial :: [Int]
            column = currentColumn partial :: [Int]
            rejectByEdgeConstraint e r = not $ lineConstraintHolds e r
    apply p@(Partial n board pos) value =
      Partial n (addToBoard p value) (advance pos)
    empty = Partial n (emptyBoard n) (0, 0)
    options = eliminate problem :: (Int, Int) -> [Int]
    n = length above

backtrack :: (a -> [b]) -> (a -> Bool) -> (a -> b -> Bool) ->
               (a -> b -> a) -> a -> [a]
backtrack grow done reject apply partial = do
  step <- grow partial
  when (reject partial step) mzero
  let partial' = apply partial step
  if done partial'
    then return partial'
    else backtrack grow done reject apply partial'

currentRow :: Partial -> [Int]
currentRow (Partial _ ([], _) _) = []
currentRow (Partial _ _ (0, _)) = []
currentRow (Partial _ (row:_, _) _) = row

currentColumn :: Partial -> [Int]
currentColumn (Partial size (_, columns) (x, _)) = columns Map.! x

addToBoard :: Partial -> Int -> PartialBoard
addToBoard (Partial size (rows, columns) (0, _)) value =
  ([value]:rows, adjust (value:) 0 columns)
addToBoard (Partial size (row:rows, columns) (x, _)) value =
  ((value:row):rows, adjust (value:) x columns)

emptyBoard n = ([], Map.fromList [(i, []) | i <- [0..n-1]])

extractSolution :: Partial -> [[Int]]
extractSolution (Partial size (rows, _) _) = reverse $ reverse <$> rows
