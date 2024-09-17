newtype Polynomial = Polynomial [Complex]

type EvenPolynomial = Polynomial

type OddPolynomial = Polynomial

type Re = Double

type Im = Double

data Complex = Complex Re Im deriving (Show, Eq)

splitPolynomial :: Polynomial -> (EvenPolynomial, OddPolynomial)
splitPolynomial (Polynomial cs) = go cs [] []
  where
    go [] even odd = (Polynomial (reverse even), Polynomial (reverse odd))
    go (c : cs) even odd = go cs odd (c : even)

fft :: Polynomial -> Polynomial
fft cs = undefined
