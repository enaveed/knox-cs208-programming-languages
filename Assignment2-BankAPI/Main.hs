module Main where

import qualified Assignment2 as A
import Network.Wai.Handler.Warp (run)

port :: Int
port = 3421

main :: IO ()
main = do
  db <- A.openDatabase "bank.db"
  putStr "Running on port: "
  print port
  run port (A.server db)
