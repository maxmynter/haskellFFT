module TestUtils where

import System.Console.ANSI
import Test.QuickCheck

colorPrint :: Color -> String -> IO ()
colorPrint color str = do
  setSGR [SetColor Foreground Vivid color]
  putStr str
  setSGR [Reset]

runNamedTest :: (Testable prop) => String -> prop -> IO Bool
runNamedTest name prop = do
  putStr $ "Testing " ++ name ++ "... "
  result <- quickCheckResult prop
  case result of
    Success {} -> do
      colorPrint Green "[PASS]\n"
      putStrLn "---------------"
      return True
    _ -> do
      colorPrint Red "[FAIL]\n"
      putStrLn "---------------"
      return False
