module Main where

import qualified Lib

main :: IO ()
main = do
  putStrLn "[LOG] Starting minecraft-dashboard application..."
  Lib.run
  putStrLn "[LOG] Finished running Lib.run"
