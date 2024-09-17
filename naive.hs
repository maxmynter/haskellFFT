module NaiveDFT where

import Data.Complex

naiveDFT :: [Complex Double] -> [Complex Double]
naiveDFT x = [sum [xn * exp (-((2 * pi * i * fromIntegral k * fromIntegral n) / fromIntegral len)) | (n, xn) <- zip [0 ..] x] | k <- [0 .. len - 1]]
  where
    len = length x
    i = 0 :+ 1

complex :: Double -> Double -> Complex Double
complex re im = re :+ im

main :: IO ()
main = do
  let input = [complex 1 0, complex 2 0, complex 3 0, complex 4 0]
  let output = naiveDFT input

  putStrLn "Polynomial"
  print input
  putStrLn "FFT's"
  mapM_ (print . roundComplex) output

roundComplex :: Complex Double -> Complex Double
roundComplex (x :+ y) = roundTo 4 x :+ roundTo 4 y
  where
    roundTo n x = fromInteger $ round $ x * (10 ^ n) / (10.0 ^^ n)
