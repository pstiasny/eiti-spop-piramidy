module Solver where
import Data.List
import Data.Maybe

import Constraints

data Piramidy = Piramidy [Maybe Int] [Maybe Int] [Maybe Int] [Maybe Int]
  deriving (Show, Read)

size :: Piramidy -> Int
size (Piramidy above _ _ _) = length above

solve :: Piramidy -> Maybe [[Int]]
solve problem = backtrack problem 0 0 (emptyBoard $ size problem)
  where backtrack :: Piramidy -> Int -> Int -> [[Maybe Int]] -> Maybe [[Int]]
        backtrack (Piramidy above below left right) i j board =
          Nothing

        emptyBoard n = [[]]
