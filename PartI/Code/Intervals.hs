module Intervals (newIvl, (+/-))  where

data Ivl = Ivl Double Double deriving Show

instance Num Ivl where
  Ivl a b + Ivl c d = Ivl (a + b) (c + d)
  Ivl a b - Ivl c d = Ivl (a - b) (c - d)
  Ivl a b * Ivl c d
      | (not . null) $ filter isNaN l = error "operation undefined -- NaN"
      | otherwise = Ivl (minimum l) (maximum l)
    where
      l = [ x * y | x <- [a, b],
                    y <- [c ,d]]

  abs i@(Ivl a b) | a >= 0    = i
                  | b <= 0    = Ivl (-b) (-a)
                  | otherwise = Ivl 0 (max (-a) b)

  signum (Ivl a b) = Ivl (signum a) (signum b)

  fromInteger i = Ivl (fromInteger i) (fromInteger i)

instance Fractional Ivl where
  fromRational r = Ivl (fromRational r) (fromRational r)

  Ivl a b / Ivl c d
    | (not . null) $ filter isNaN l = error "operation undefined -- NaN"
    | c == 0 || d == 0              = error "operation undefined -- divided by zero"
    | otherwise                     = Ivl (minimum l) (maximum l)
    where
      l = [ x / y | x <- [a, b],
                    y <- [c, d]]

(+/-) :: Double -> Double -> Ivl
(+/-) a b = Ivl (min x y) (max x y)
  where
    x = a + b
    y = a - b

newIvl :: Double -> Double -> Ivl
newIvl a b | a <= b = Ivl a b
           | otherwise = error "lower bound cannot be greater than upper bound"

-- for testing
instance Eq Ivl where
  Ivl a b == Ivl c d = a == c && b == d

testIvl :: Bool
testIvl = (-3) +/- (-4) == Ivl (-7) 1   &&
          1 +/- 0.5     == Ivl 0.5 1.5  &&
          1 +/- 2       == Ivl (-1) 3
