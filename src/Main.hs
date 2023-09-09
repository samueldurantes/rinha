module Main where

import Tree (File)
import Data.Aeson (eitherDecode)

import qualified Data.ByteString.Lazy.Char8 as LBS

main :: IO ()
main = do
  json <- LBS.readFile "examples/fib.json"
  case eitherDecode json of
    Left err -> putStrLn $ "Error: " ++ err
    Right value -> print (value :: File)
