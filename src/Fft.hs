module Fft where

newtype Polynomial = Polynomial [Complex] deriving (Show)

type EvenPolynomial = Polynomial

type OddPolynomial = Polynomial

type Re = Double

type Im = Double

type Mag = Double

type Phi = Double

data Complex = Cartesian Re Im | Polar Mag Phi deriving (Show, Eq)

im :: Complex -> Double
im (Cartesian _ i) = i

real :: Complex -> Double
real (Cartesian r _) = r

mag :: Complex -> Double
mag (Cartesian r i) = sqrt $ r * r + i * i
mag (Polar m p) = m

phase :: Complex -> Double
phase (Polar _ phi) = phi
phase (Cartesian r i) = atan2 r i

asCartesian :: Complex -> Complex
asCartesian c = Cartesian (real c) (im c)

asPolar :: Complex -> Complex
asPolar c = Polar (mag c) (phase c)

instance Num Complex where
  (+) (Cartesian a b) (Cartesian r i) = Cartesian (a + r) (b + i)
  (+) c1 c2 = asCartesian c1 + asCartesian c2
  (*) (Polar r p) (Polar m t) = Polar (r * m) (p + t)
  (*) c1 c2 = asPolar c1 * asPolar c2
  negate (Cartesian a b) = Cartesian (-a) (-b)
  negate (Polar r p) = Polar (-r) p
  signum c@(Cartesian a b) = let m = mag c in Cartesian (a / m) (b / m)
  signum (Polar r p) = signum r
  fromInteger n = Cartesian (fromInteger n) 0
  abs c@(Cartesian a b) = Cartesian (mag c) 0

instance Show Complex where
  show (Cartesian r i) = show r ++ " + " show y ++ "i"
  show (Polar r p) = show r ++ "∠" ++ show p

splitPolynomial :: Polynomial -> (EvenPolynomial, OddPolynomial)
splitPolynomial (Polynomial cs) = (Polynomial (evenPart cs), Polynomial (oddPart cs))
  where
    evenPart [] = []
    evenPart [x] = [x]
    evenPart (x : _ : xs) = x : evenPart xs

    oddPart [] = []
    oddPart [_] = []
    oddPart (_ : y : ys) = y : oddPart ys

unityRoots :: Int -> Int -> Complex
unityRoots n = [exp ((2 * pi * k * Complex 0 1) / n) | k <- [0 .. (n - 1)]]

fft :: Polynomial -> Polynomial
fft p@(Polynomial []) = p
fft cs = undefined
