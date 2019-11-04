{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Monoid
import Data.Semigroup

type Position = (Double, Double)

newtype Length = Length { getLength :: Double } deriving (Eq, Ord, Num, Show)
newtype Area   = Area   { getArea   :: Double } deriving (Eq, Ord, Num, Show)


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
    asCount :: Sum Int,
    asSumArea :: Sum Area,
    asSumCircumference :: Sum Length,
    asMaxArea :: Max Area,
    asMaxCircumference :: Max Length
  } deriving Show

area :: Object -> Area
area (Rectangle{ .. }) = Area (getLength width * getLength height)
area (Circle{ .. })    = Area (pi * getLength radius * getLength radius)

circum :: Object -> Length
circum (Rectangle { .. }) = Length ( (getLength width + getLength height) * 2 )
circum (Circle { .. })    = Length ( pi * getLength radius * 2 )


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
    avgArea = Area ( (getArea $ getSum $ asSumArea accum)
                     / (fromIntegral $ getSum $ asCount accum) ),
    avgCircumference = Length ( (getLength $ getSum $ asSumCircumference accum )
                                / (fromIntegral $ getSum $ asCount accum) ),
    maxArea = getMax $ asMaxArea accum,
    maxCircumference = getMax $ asMaxCircumference accum }
  where
    accum = foldMap accumStats' dobject

accumStats' :: Object -> AccumStats
accumStats' object =
  AccumStats {
    asCount = 1,
    asSumArea = Sum (area object),
    asSumCircumference = Sum (circum object),
    asMaxArea = Max (area object),
    asMaxCircumference = Max (circum object) }


statistics :: Drawing Object -> Statistics
statistics dobject =
  Statistics {
    avgArea = Area ( (getArea $ getSum $ asSumArea accum)
                     / (fromIntegral $ getSum $ asCount accum) ),
    avgCircumference = Length ( (getLength $ getSum $ asSumCircumference accum )
                                / (fromIntegral $ getSum $ asCount accum) ),
    maxArea = getMax $ asMaxArea accum,
    maxCircumference = getMax $ asMaxCircumference accum }
  where
    accum = accumStats (AccumStats 0 0 0 0 0) dobject


accumStats :: AccumStats -> Drawing Object -> AccumStats
accumStats accum (Element object) =
  AccumStats {
    asCount = asCount accum + 1,
    asSumArea = Sum ( Area (
                        (getArea $ getSum $ asSumArea accum) +
                        ( getArea $ area object) ) ),
    asSumCircumference = Sum ( Length (
                                (getLength $ getSum $ asSumCircumference accum) +
                                 (getLength $ circum object) ) ),
    asMaxArea = Max ( Area (
                        max (getArea $ getMax $ asMaxArea accum)
                            (getArea $ area object) )),
    asMaxCircumference = Max ( Length (
                                 max (getLength $ getMax $ asMaxCircumference accum)
                                   (getLength $ circum object))) }
accumStats accum (Group [])     = accum
accumStats accum (Group (x:xs)) = accumStats (accumStats accum x) (Group xs)


