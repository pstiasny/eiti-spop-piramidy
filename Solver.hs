module Solver (Piramidy (Piramidy), solve) where
import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace

data Piramidy = Piramidy [Maybe Int] [Maybe Int] [Maybe Int] [Maybe Int]
  deriving (Show, Read)

type PartialBoard = [(Int, Int, Int)]
data Partial = Partial Int PartialBoard (Int, Int) deriving Show

solve :: Piramidy -> Maybe [[Int]]
solve (Piramidy [] [] [] []) = Just []
solve (Piramidy above below left right) =
  listToMaybe $ extractSolution <$> backtrack grow done reject apply empty
  where
    grow (Partial n board pos) = [1..n]
    advance (x, y) = let (dx, x') = (x + 1) `quotRem` n in (x', y + dx)
    done (Partial n _ (x, y)) = (x, y) == (0, n)
    reject partial@(Partial n board (x,y)) value =
      value `elem` row ||
      value `elem` column ||
      (if y == n-1
        then rejectByEdgeConstraint (above !! x) (column ++ [value]) ||
             rejectByEdgeConstraint (below !! x) (value:reverse (column))
        else False) ||
      (if x == n-1
        then rejectByEdgeConstraint (left !! y) (row ++ [value]) ||
             rejectByEdgeConstraint (right !! y) (value:reverse row)
        else False)

      where row = currentRow partial
            column = currentColumn partial
            rejectByEdgeConstraint e r = not $ lineConstraintHolds e r
    apply p@(Partial n board pos) value =
      Partial n (addToBoard p value) (advance pos)
    empty = Partial n [] (0, 0)
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

extractRow board y =
  map (\(_, _, v) -> v) $
    sortBy (\(x, _, _) (x', _, _) -> compare x x') $ 
    filter (\(_,y',_) -> y == y') board
currentRow (Partial size board (x, y)) = extractRow board y
currentColumn (Partial size board (x, y)) =
  map (\(_, _, v) -> v) $ 
    sortBy (\(_, y, _) (_, y', _) -> compare y y') $ 
    filter (\(x',_,_) -> x == x') board
addToBoard :: Partial -> Int -> PartialBoard
addToBoard (Partial size board (x, y)) value = (x,y,value):board

extractSolution :: Partial -> [[Int]]
extractSolution (Partial size board _) = 
  map (extractRow board) [0..size-1]

-- Check if a row or column adheres to a constraint on its
-- beginning, i.e. do I see i pyramids from the point of list's
-- beginning.  Always True if there is no constraint.
lineConstraintHolds :: Maybe Int -> [Int] -> Bool
lineConstraintHolds Nothing _ = True
lineConstraintHolds (Just i) xs = lineConstraintFor xs == i

-- How many pyrmids can be seen from the point of list's beginning.
lineConstraintFor :: [Int] -> Int
lineConstraintFor line = inner 0 0 line
  where inner _ i [] = i
        inner max i (h:t) = if h > max then inner h (i+1) t
                                       else inner max i t
