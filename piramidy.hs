import Control.Monad
import Data.List (last)
import System.Environment (getArgs)

import Problem
import Solver

main = do
  args <- getArgs
  if length args < 1
    then error "usage: piramidy <input file>"
    else do
      let fileName = last args
          prettyPrinting = "--pretty" `elem` args

      description <- readFile fileName
      let problem = read description :: Piramidy
          solution = solve problem

      if prettyPrinting
        then prettyPrint problem solution
        else print solution
