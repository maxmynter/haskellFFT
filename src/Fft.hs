module Fft where

newtype Polynomial = Polynomial [Complex] deriving (Show)

type EvenPolynomial = Polynomial

type OddPolynomial = Polynomial

type Re = Double

type Im = Double

type Mag = Double

type Phi = Double

data Complex = Cartesian Re Im | Polar Mag Phi deriving (Eq)

i :: Complex
i = Cartesian 0 1

im :: Complex -> Double
im (Cartesian _ img) = img
im (Polar r phi) = r * sin phi

real :: Complex -> Double
real (Cartesian r _) = r
real (Polar r phi) = r * cos phi

mag :: Complex -> Double
mag (Cartesian r img) = sqrt $ r * r + img * img
mag (Polar m _) = m

phase :: Complex -> Double
phase (Polar _ phi) = phi
phase (Cartesian r img) = atan2 img r

asCartesian :: Complex -> Complex
asCartesian c = Cartesian (real c) (im c)

asPolar :: Complex -> Complex
asPolar c = Polar (mag c) (phase c)

complexFromInt :: Int -> Complex
complexFromInt n = Cartesian (fromIntegral n) 0

complexFromDouble :: Double -> Complex
complexFromDouble d = Cartesian d 0

instance Num Complex where
  (+) (Cartesian a b) (Cartesian r img) = Cartesian (a + r) (b + img)
  (+) c1 c2 = asCartesian c1 + asCartesian c2
  (*) (Polar r p) (Polar m t) = Polar (r * m) (p + t)
  (*) c1 c2 = asPolar c1 * asPolar c2
  negate (Cartesian a b) = Cartesian (-a) (-b)
  negate (Polar r p) = Polar (-r) p
  signum c@(Cartesian a b) = let m = mag c in Cartesian (a / m) (b / m)
  signum cp@(Polar _ _) = signum $ asCartesian $ signum cp
  fromInteger n = Cartesian (fromInteger n) 0
  abs c = Cartesian (mag c) 0

instance Show Complex where
  show (Cartesian r img) = show r ++ " + " ++ show img ++ "i"
  show (Polar r p) = show r ++ "∠" ++ show p

instance Floating Complex where
  pi = Cartesian pi 0
  exp (Cartesian a b) = Polar (exp a) b
  log (Cartesian a b) = Cartesian (log $ sqrt $ a * a + b * b) (atan2 b a)
  sin (Cartesian a b) = Cartesian (sin a * cosh b) (cos a * sinh b)
  cos (Cartesian a b) = Cartesian (cos a * cosh b) (-sin a * sinh b)

instance Fractional Complex where
  (/) (Cartesian a b) (Cartesian c d) = let denum = c * c + d * d in Cartesian ((a * c + b * d) / denum) ((b * c - a * d) / denum)

splitPolynomial :: Polynomial -> (EvenPolynomial, OddPolynomial)
splitPolynomial (Polynomial cs) = (Polynomial (evenPart cs), Polynomial (oddPart cs))
  where
    evenPart [] = []
    evenPart [x] = [x]
    evenPart (x : _ : xs) = x : evenPart xs

    oddPart [] = []
    oddPart [_] = []
    oddPart (_ : y : ys) = y : oddPart ys

unityRoots :: Int -> [Complex]
unityRoots n = [exp ((2 * complexFromDouble pi * complexFromInt k * i) / complexFromInt n) | k <- [0 .. (n - 1)]]

fft :: Polynomial -> Polynomial
fft p@(Polynomial []) = p
fft cs = undefined
