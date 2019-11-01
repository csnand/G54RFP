{-# LANGUAGE RecordWildCards #-}

-- task I.1 hamming list
merge :: [Int] -> [Int] -> [Int]
merge [] _ = []
merge _ [] = []
merge xss@(x:xs) yss@(y:ys)
  | x == y = x : merge xs ys
  | x < y  = x : merge xs yss
  | x > y  = y : merge xss ys


hamming :: [Int]
hamming = 1 : merge (map (2*) hamming) (merge (map (3*) hamming) (map (5*) hamming))


-- task I.4 interval arithmetic
-- TODO : enforce the invariant -- using by exposing
-- specific constructor

data Ivl = Ivl Double Double deriving Show

instance Num Ivl where
  Ivl a b + Ivl c d = Ivl (a + b) (c + d)
  Ivl a b - Ivl c d = Ivl (a - b) (c - d)
  Ivl a b * Ivl c d = Ivl (minimum l) (maximum l)
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

  Ivl a b / Ivl c d = Ivl (minimum l) (maximum l)
    where
      l = [ x / y | x <- [a, b],
                    y <- [c, d]]

(+/-) :: Double -> Double -> Ivl
(+/-) a b = undefined


-- tests for task I.4
testIvl :: Bool
testIvl = undefined


-- task I.5
type Position = (Double, Double)
type Length = Double
type Area = Double

data Object =
   Rectangle {
     centre :: Position,
     width :: Length,
     height :: Length }
 | Circle {
     centre :: Position,
     radius :: Length }

data Drawing a = Element a | Group [Drawing a]

data Statistics =
  Statistics {
    avgArea :: Area,
    avgCircumference :: Length,
    maxArea :: Area,
    maxCircumference :: Length
  } deriving Show

data AccumStats =
  AccumStats {
    asCount :: Int,
    asSumArea :: Area,
    asSumCircumference :: Length,
    asMaxArea :: Area,
    asMaxCircumference :: Length
  }

-- task I.5 1
statistics :: Drawing Object -> Statistics
statistics dobject =
  Statistics {
    avgArea = asSumArea accum / (fromIntegral $ asCount accum),
    avgCircumference = asSumCircumference accum / (fromIntegral $ asCount accum),
    maxArea = asMaxArea accum,
    maxCircumference = asMaxCircumference accum }
  where
    accum = accumStats (AccumStats 0 0 0 0 0) dobject


accumStats :: AccumStats -> Drawing Object -> AccumStats
accumStats accum (Element object) =
  AccumStats {
    asCount = asCount accum + 1,
    asSumArea = asSumArea accum + area object,
    asSumCircumference = asSumCircumference accum + circum object,
    asMaxArea = max (asMaxArea accum) (area object),
    asMaxCircumference = max (asMaxCircumference accum) (circum object) }

accumStats accum (Group (x:xs)) = accumStats (accumStats accum x) (Group xs)


area :: Object -> Area
area (Rectangle{ .. }) = width * height
area (Circle{ .. })    = pi * radius * radius

circum :: Object -> Length
circum (Rectangle { .. }) = (width + height) /2
circum (Circle { .. })    = pi * radius * 2

-- task I.5 2

instance Foldable Drawing where
  foldMap f (Element object) = f object
  foldMap f (Group (x:xs)) = foldMap f x <> foldMap f (Group xs)

instance Semigroup AccumStats where
  a <> b =
    AccumStats {
      asCount = asCount a + asCount b,
      asSumArea = asSumArea a + asSumArea b,
      asSumCircumference = asSumCircumference a + asSumCircumference b,
      asMaxArea = max (asMaxArea a) (asMaxArea b),
      asMaxCircumference = max (asMaxCircumference a) (asMaxCircumference b) }

instance Monoid AccumStats where
  mempty = AccumStats 0 0 0 0 0

foldMapStatistics :: Drawing Object -> Statistics
foldMapStatistics dobject =  Statistics {
    avgArea = asSumArea accum / (fromIntegral $ asCount accum),
    avgCircumference = asSumCircumference accum / (fromIntegral $ asCount accum),
    maxArea = asMaxArea accum,
    maxCircumference = asMaxCircumference accum }
  where
    accum = foldMap (accumStats mempty) dobject



