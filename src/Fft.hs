module Fft where

newtype Polynomial = Polynomial [Complex] deriving (Show)

type EvenPolynomial = Polynomial

type OddPolynomial = Polynomial

type Re = Double

type Im = Double

type Mag = Double

type Phi = Double

data Complex = Cartesian Re Im | Polar Mag Phi

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

normalizePhase :: Phi -> Phi
normalizePhase p
  | p > pi = normalizePhase (p - 2 * pi)
  | p <= -pi = normalizePhase (p + 2 * pi)
  | otherwise = p

phase :: Complex -> Double
phase (Polar r phi) = if r == 0 then 0 else normalizePhase phi
phase z@(Cartesian r img) = if mag z == 0 then 0 else normalizePhase $ atan2 img r

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

  (*) (Polar r p) (Polar m t) = Polar (r * m) (normalizePhase $ p + t)
  (*) c1 c2 = asPolar c1 * asPolar c2

  negate (Cartesian a b) = Cartesian (-a) (-b)
  negate (Polar r p) = Polar (-r) p

  signum c@(Cartesian a b) = let m = mag c in if m < 1e-6 then Cartesian 0 0 else Cartesian (a / m) (b / m)
  signum p@(Polar _ _) = signum $ asCartesian p

  fromInteger n = Cartesian (fromInteger n) 0
  abs c = Cartesian (mag c) 0

instance Show Complex where
  show (Cartesian r img) = show r ++ " + " ++ show img ++ "i"
  show (Polar r p) = show r ++ "∠(rad)" ++ show p

instance Eq Complex where
  (==) (Cartesian r1 i1) (Cartesian r2 i2) = r1 == r2 && i1 == i2
  (==) (Polar r1 p1) (Polar r2 p2)
    | r1 == 0 && r2 == 0 = True
    | otherwise = r1 == r2 && normalizePhase p1 == normalizePhase p2
  (==) z1 z2 = asPolar z1 == asPolar z2

instance Floating Complex where
  pi = Cartesian pi 0
  exp z = Polar (exp (real z)) (im z)
  log z = Cartesian (log $ sqrt $ a * a + b * b) (atan2 b a)
    where
      a = real z
      b = im z
  sin z = Cartesian (sin a * cosh b) (cos a * sinh b)
    where
      a = real z
      b = im z
  cos z = Cartesian (cos a * cosh b) (-(sin a * sinh b))
    where
      a = real z
      b = im z
  sinh z = Cartesian (sinh a * cos b) (cosh a * sin b)
    where
      a = real z
      b = im z
  cosh z = Cartesian (cosh a * cos b) (sinh a * sin b)
    where
      a = real z
      b = im z
  atan z = i / 2 * log ((i + z) / (i - z))
  asinh z = log (z + sqrt (z * z + 1))
  acosh z = log (z + sqrt (z * z - 1))
  atanh z = log ((1 + z) / (1 - z)) / 2
  asin z = -(i * log (i * z + sqrt (1 - z * z)))
  acos z = pi / 2 - asin z

instance Fractional Complex where
  (/) (Cartesian a b) (Cartesian c d) = let denum = c * c + d * d in Cartesian ((a * c + b * d) / denum) ((b * c - a * d) / denum)
  (/) (Polar r p) (Polar m t) = Polar (r / m) (normalizePhase (p - t))
  (/) c1 c2 = asCartesian c1 / asCartesian c2
  fromRational r = Cartesian (fromRational r) 0

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
fft p@(Polynomial cs) =
  let (evens, odds) = splitPolynomial p
      n = length cs `div` 2
      Polynomial xEven = fft evens
      Polynomial xOdds = fft odds
      roots = unityRoots n
   in Polynomial $
        [xEven !! k + roots !! k * xOdds !! k | k <- [0 .. n - 1]]
          ++ [xEven !! k - roots !! k * xOdds !! k | k <- [0 .. n - 1]]
