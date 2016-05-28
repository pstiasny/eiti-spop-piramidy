module Problem where
import Control.Monad
import Data.List

data Piramidy = Piramidy [Maybe Int] [Maybe Int] [Maybe Int] [Maybe Int]
  deriving (Show, Read)


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

prettyPrint (Piramidy above below left right) solution =
  case solution of
    Nothing -> putStrLn "no solution"
    Just rows -> do
      putStr " "
      forM_ above (putStr . prettyConstr)
      putStrLn ""

      forM_ (zip3 left rows right) $ \(l, row, r) -> do
          putStr $ prettyConstr l
          forM_ row (putStr . show)
          putStrLn $ prettyConstr r

      putStr " "
      forM_ below (putStr . prettyConstr)
      putStrLn ""

  where prettyConstr (Nothing) = " "
        prettyConstr (Just i) = show i
