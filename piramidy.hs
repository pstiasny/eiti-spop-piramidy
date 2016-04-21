import System.Environment (getArgs)
import Solver

main = do
  args <- getArgs
  if length args /= 1
    then error "usage: piramidy <input file>"
    else do
      let [fileName] = args
      description <- readFile fileName
      let problem = read description :: Piramidy
      print $ solve problem
