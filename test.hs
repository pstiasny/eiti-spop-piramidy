{-# LANGUAGE TemplateHaskell #-}
import Data.Maybe
import Data.List
import Control.Monad
import Test.QuickCheck
import Problem
import Solver

instance Arbitrary Piramidy where
  arbitrary = sized arbPuzzle

-- Generator of arbitrary puzzles of size `size`.
arbPuzzle :: Int -> Gen Piramidy
arbPuzzle size = do
  let makeContraints = do
        v <- vectorOf size (choose (1, size))
        mapM (\i -> elements [Just i, Nothing]) v
  [above, below, left, right] <- replicateM 4 makeContraints
  return $ Piramidy above below left right

-- Generator of solvable puzzles of given size.
solvablePuzzle :: Int -> Gen Piramidy
solvablePuzzle size = do
  -- Generator of arbitrary legal boards
  let anyBoard = do
        let makeRow = shuffle [1..size]
            rows = vectorOf size makeRow
        suchThat rows $ (all isNonrepeatingLine . transpose)

  -- Find number of pyramids seen anywhere on board's edges
  let contraintsFor rows =
        Piramidy
          (map (Just . lineConstraintFor) columns)
          (map (Just . lineConstraintFor) $ map reverse columns)
          (map (Just . lineConstraintFor) rows)
          (map (Just . lineConstraintFor) $ map reverse rows)
        where columns = transpose rows

  -- Remove constraints randomly
  let reduceConstraints (Piramidy a b l r) = do
        a' <- mapM re a
        b' <- mapM re b
        l' <- mapM re l
        r' <- mapM re r
        return $ Piramidy a' b' l' r'
        where re (Just x) = do
                elements [Just x, Nothing]
              re Nothing = return Nothing

  board <- anyBoard
  let fullConstraints = contraintsFor board
  reduceConstraints fullConstraints

isNonrepeatingLine xs = [1..length xs] \\ xs == []

-- Find boards holding puzzle's constraints out of all
-- possible boards.
bruteforce p@(Piramidy a b l r) =
  filter (constraintsHold p) $ allMatrices
  where allMatrices = filter nonrep $ sequence $ replicate size allRows
        nonrep rows = all isNonrepeatingLine (transpose rows)
        allRows = permutations [1..size]
        size = length a

prop_solveMatchesBruteForcedSolution = mapSize (min 4) $ \p ->
  let bruteSolutions = bruteforce p
  in  (length bruteSolutions <= 1) ==>
        classify (null bruteSolutions) "no solution" $
          solve p == listToMaybe bruteSolutions

prop_solveReturnsSolutionWithinConstraints =
  forAll (sized solvablePuzzle) $ \p ->
    case solve p of
      Just solution -> constraintsHold p solution
      Nothing -> False

prop_solveReturnsLegalBoard =
  forAll (sized solvablePuzzle) $ \p ->
    case solve p of
      Just rows -> all isNonrepeatingLine rows &&
                   all isNonrepeatingLine (transpose rows)
      Nothing -> False


return []  -- template haskell magic for quickcheck
main =
  $forAllProperties (quickCheckWithResult stdArgs {maxSize=6, maxSuccess=200})
