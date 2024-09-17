module Fft where

newtype Polynomial = Polynomial [Complex] deriving (Show)

type EvenPolynomial = Polynomial

type OddPolynomial = Polynomial

type Re = Double

type Im = Double

data Complex = Complex Re Im deriving (Show, Eq)

im :: Complex -> Double
im (Complex _ i) = i

real :: Complex -> Double
real (Complex r _) = r

instance Num Complex where
  (+) (Complex a b) (Complex r i) = Complex (a + r) (b + i)
  (*) (Complex a b) (Complex r i) = Complex (a * r - b * i) (a * i + b * r)
  negate (Complex a b) = Complex (-a) (-b)
  signum c@(Complex a b) = let mag = real c in Complex (a / mag) (b / mag)
  fromInteger n = Complex (fromInteger n) 0
  abs (Complex a b) = Complex (sqrt (a * a + b * b)) 0

splitPolynomial :: Polynomial -> (EvenPolynomial, OddPolynomial)
splitPolynomial (Polynomial cs) = (Polynomial (evenPart cs), Polynomial (oddPart cs))
  where
    evenPart [] = []
    evenPart [x] = [x]
    evenPart (x : _ : xs) = x : evenPart xs

    oddPart [] = []
    oddPart [_] = []
    oddPart (_ : y : ys) = y : oddPart ys

fft :: Polynomial -> Polynomial
fft cs = undefined
