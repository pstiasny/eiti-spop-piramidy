module Solver where
import Data.List
import Data.Maybe

data Piramidy = Piramidy [Maybe Int] [Maybe Int] [Maybe Int] [Maybe Int]
  deriving (Show, Read)

solve :: Piramidy -> Maybe [[Int]]
solve (Piramidy above below left right) =
  -- BEGIN STUB --
  -- Zaślepkowe rozwiązanie.  Tu wstaw właściwy alogrytm.
  listToMaybe $ bruteforce (Piramidy above below left right)
  where
    lineConstraintHolds :: Maybe Int -> [Int] -> Bool
    lineConstraintHolds Nothing _ = True
    lineConstraintHolds (Just i) xs = inner 0 i xs
      where inner _ 0 [] = True
            inner _ _ [] = False
            inner max i (h:t) = if h > max then inner h (i-1) t
                                           else inner max i t

    constraintsHold :: Piramidy -> [[Int]] -> Bool
    constraintsHold (Piramidy above below left right) rows =
      all id $
        zipWith lineConstraintHolds left rows ++
        zipWith lineConstraintHolds right (map reverse rows) ++
        zipWith lineConstraintHolds above columns ++
        zipWith lineConstraintHolds below (map reverse columns)
      where columns = transpose rows

    isNonrepeatingLine xs = [1..length xs] \\ xs == []

    bruteforce p@(Piramidy a b l r) =
      filter (constraintsHold p) $ allMatrices
      where allMatrices = filter nonrep $ sequence $ replicate size allRows
            nonrep rows = all isNonrepeatingLine (transpose rows)
            allRows = permutations [1..size]
            size = length a
  -- END STUB --
