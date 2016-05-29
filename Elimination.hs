module Elimination (eliminate) where

import Control.Monad
import Data.List
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Problem


eliminate :: Piramidy -> (Int, Int) -> [Int]
eliminate (Piramidy above below left right) =
  let 
    size = length above
    index x y = y * size + x

    v = V.create $ do
      board <- MV.replicate (size * size) [1..size]

      let
        -- row/column accessors and modifiers
        read x y = MV.read board (index x y)
        write x y v = MV.write board (index x y) v

        rid badOptions x y = do
          options <- read x y
          let options' = options \\ badOptions
          write x y options'
          when (options /= options' && length options' == 1) $ do
            forM_ [0..x - 1] $ \x -> rid options' x y
            forM_ [x + 1..size-1] $ \x -> rid options' x y
            forM_ [0..y - 1] $ \y -> rid options' x y
            forM_ [y + 1..size-1] $ \y -> rid options' x y

        eliminateRow edgeNumber row = do
          forM_ row $ \(i, x, y) -> do
            rid [i + size + 1 - edgeNumber..size] x y

      forM_ (zip left [0..size-1]) $ \(mk, y) -> maybeDo mk $ \k ->
        eliminateRow k [(i, i - 1, y) | i <- [1..k - 1]]
      forM_ (zip right [0..size-1]) $ \(mk, y) -> maybeDo mk $ \k ->
        eliminateRow k [(i, size - i, y) | i <- [1..k - 1]]
      forM_ (zip above [0..size-1]) $ \(mk, x) -> maybeDo mk $ \k ->
        eliminateRow k [(i, x, i - 1) | i <- [1..k - 1]]
      forM_ (zip below [0..size-1]) $ \(mk, x) -> maybeDo mk $ \k ->
        eliminateRow k [(i, x, size - i) | i <- [1..k - 1]]
      return board
  in \(x, y) -> v V.! index x y

maybeDo (Just x) f = f x
maybeDo Nothing _ = return ()
