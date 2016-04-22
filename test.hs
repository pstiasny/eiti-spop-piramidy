import Data.Maybe
import Data.List
import Control.Monad
import Test.QuickCheck
import Solver

instance Arbitrary Piramidy where
  arbitrary = sized arbPuzzle

-- Generator of arbitrary puzzles of size `size`.
arbPuzzle :: Int -> Gen Piramidy
arbPuzzle size = do
  let makeContraints = do
        v <- vectorOf size (choose (1, size))
        constrs <- mapM (\i -> elements [Just i, Nothing]) v
        return constrs
  [above, below, left, right] <- replicateM 4 makeContraints
  return $ Piramidy above below left right
  
-- Check if a row or column adheres to a constraint on its
-- beginning, i.e. do I see i piramids from the point of list's
-- beginning.  Always True if there is no constraint.
lineConstraintHolds :: Maybe Int -> [Int] -> Bool
lineConstraintHolds Nothing _ = True
lineConstraintHolds (Just i) xs = inner 0 i xs
  where inner _ 0 [] = True
        inner _ _ [] = False
        inner max i (h:t) = if h > max then inner h (i-1) t
                                       else inner max i t

-- Check if every row and column adheres to the constraints
-- on board's edges.
constraintsHold :: Piramidy -> [[Int]] -> Bool
constraintsHold (Piramidy above below left right) rows =
  all id $
    zipWith lineConstraintHolds left rows ++
    zipWith lineConstraintHolds right (map reverse rows) ++
    zipWith lineConstraintHolds above columns ++
    zipWith lineConstraintHolds below (map reverse columns)
  where columns = transpose rows

isNonrepeatingLine xs = [1..length xs] \\ xs == []

-- Find boards holding puzzle's constraints out of all
-- possible boards.
bruteforce p@(Piramidy a b l r) =
  filter (constraintsHold p) $ allMatrices
  where allMatrices = filter nonrep $ sequence $ replicate size allRows
        nonrep rows = all isNonrepeatingLine (transpose rows)
        allRows = permutations [1..size]
        size = length a

main = do
  verboseCheckWith
    (stdArgs {maxSize=5, maxSuccess=200})
    (\p ->
      let bruteSolutions = bruteforce p
      in  (length bruteSolutions <= 1) ==>
            classify (null bruteSolutions) "no solution" $
              solve p == listToMaybe bruteSolutions)
