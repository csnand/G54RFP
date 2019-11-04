{-# LANGUAGE RecordWildCards #-}

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
   deriving Show

data Drawing a = Element a | Group [Drawing a] deriving Show

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
  } deriving Show

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
accumStats accum (Group [])     = accum
accumStats accum (Group (x:xs)) = accumStats (accumStats accum x) (Group xs)


area :: Object -> Area
area (Rectangle{ .. }) = width * height
area (Circle{ .. })    = pi * radius * radius

circum :: Object -> Length
circum (Rectangle { .. }) = (width + height) * 2
circum (Circle { .. })    = pi * radius * 2

-- task I.5 2

instance Foldable Drawing where
  foldMap f (Element object) = f object
  foldMap f (Group [])       = mempty
  foldMap f (Group (x:xs))   = foldMap f x <> foldMap f (Group xs)

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
    accum = foldMap accumStats' dobject

accumStats' :: Object -> AccumStats
accumStats' object =
  AccumStats {
    asCount = 1,
    asSumArea = area object,
    asSumCircumference = circum object,
    asMaxArea = area object,
    asMaxCircumference = circum object }

