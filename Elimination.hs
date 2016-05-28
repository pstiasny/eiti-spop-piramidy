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
        -- indexing within rows and columns
        row board y = (board, \x -> index x y)
        column board x = (board, index x)
        flip (board, f) = (board, \x -> f (size - x - 1))

        -- row/column accessors and modifiers
        modify (board, index) f x = MV.modify board f (index x)
        {-read row x = MV.read board (row x)-}

        eliminateRow Nothing _ = return ()
        eliminateRow (Just edgeNumber) row = do
          forM_ [1..edgeNumber - 1] $ \i -> do
            modify row (\\ [i + size + 1 - edgeNumber..size]) (i - 1)

      forM_ (zip left [0..size-1]) $ \(edge, y) ->
        eliminateRow edge (row board y)
      forM_ (zip right [0..size-1]) $ \(edge, y) ->
        eliminateRow edge (flip $ row board y)
      forM_ (zip above [0..size-1]) $ \(edge, x) ->
        eliminateRow edge (column board x)
      forM_ (zip below [0..size-1]) $ \(edge, x) ->
        eliminateRow edge (flip $ column board x)
      return board
  in \(x, y) -> v V.! index x y
