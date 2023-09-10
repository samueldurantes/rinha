module Main where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Evaluator (eval)
import qualified Tree

main :: IO ()
main = do
  json <- LBS.readFile "examples/fib.json"
  case eitherDecode json of
    Left err -> putStrLn $ "Error: " ++ err
    Right (file :: Tree.File) -> do
      eval [] (Tree.fileExpression file)
      pure ()
