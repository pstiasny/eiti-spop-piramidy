module Solver where

data Piramidy = Piramidy [Maybe Int] [Maybe Int] [Maybe Int] [Maybe Int]
  deriving (Show, Read)

solve :: Piramidy -> Maybe [[Int]]
solve (Piramidy above below left right) =
  Nothing
