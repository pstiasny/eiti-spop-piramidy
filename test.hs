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
        suchThat rows (all isNonrepeatingLine . transpose)

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

noMoreThan = min

-- Check if, for a random puzzle, solver's solutions matches that
-- of an exhaustive search.  We limit puzzle size since it is
-- restricted by exhauistive search speed.
prop_solveMatchesBruteForcedSolution = mapSize (noMoreThan 4) $ \p ->
  let bruteSolutions = bruteforce p
      solution = solve p
  in  (length bruteSolutions <= 1) ==>
        classify (null bruteSolutions) "no solution" $
        counterexample ("incorrect solution: " ++ show solution) $
          solution == listToMaybe bruteSolutions

-- Check if, for a solvable puzzle, solver returns a solution
-- that matches constraints on puzzle's edges.  We limit maximum
-- problem size as speed is restricted by the problem generator.
prop_solveReturnsSolutionWithinConstraintsForSolvableBoards =
  mapSize (noMoreThan 4) $
  forAll (sized solvablePuzzle) $ \p ->
    trySolve p
      (\board -> constraintsHold p board)
      False

-- Check if, for a any random puzzle, solver returns a solution
-- that matches constraints on puzzle's edges if a solution is found.
prop_solveReturnsSolutionWithinConstraints p =
  trySolve p
    (\board -> constraintsHold p board)
    True

-- Check if, for a solvable puzzle, solver returns a solution
-- that conforms to the no repetetion of numbers per row/column
-- rule.  We limit maximum problem size as speed is restricted by
-- the problem generator.
prop_solveReturnsLegalBoardForSolvableBoards =
  mapSize (noMoreThan 4) $
  forAll (sized solvablePuzzle) $ \p ->
    trySolve p
      (\rows -> all isNonrepeatingLine rows &&
                all isNonrepeatingLine (transpose rows))
      False

-- Check if, for any random puzzle, solver returns a solution
-- that conforms to the no repetetion of numbers per row/column
-- rule if a solution is found.
prop_solveReturnsLegalBoard p =
  trySolve p
    (\rows -> all isNonrepeatingLine rows &&
              all isNonrepeatingLine (transpose rows))
    True

trySolve p onJust onNothing =
  let solution = solve p
  in
    classify (isNothing solution) "no solution" $
    counterexample ("incorrect solution: " ++ show solution) $
    case solution of
      Just board -> onJust board
      Nothing -> onNothing

return []  -- template haskell magic for quickcheck
main =
  $forAllProperties (quickCheckWithResult stdArgs {maxSize=7, maxSuccess=200})
