import Control.Monad
import Data.List (last)
import System.Environment (getArgs)

import Solver

main = do
  args <- getArgs
  if length args < 1
    then error "usage: piramidy <input file>"
    else do
      let fileName = last args
          prettyPrinting = "--pretty" `elem` args

      description <- readFile fileName
      let problem@(Piramidy above below left right) = read description :: Piramidy
          solution = solve problem

          prettyConstr (Nothing) = " "
          prettyConstr (Just i) = show i

      if prettyPrinting
      then do
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
      else
        print solution
