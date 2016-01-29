{-# OPTIONS -fno-warn-type-defaults -fwarn-incomplete-patterns -Wall -fwarn-tabs -fno-warn-type-defaults #-}
module BinPack where

type Width = Double
type Height = Double

data Bin = B [Rectangle] 
   deriving (Show,Eq)

data Result = R [Bin]
   deriving (Show,Eq)

type Point = (Double, Double)

data Shape = Polygon [Point]
           | Stock Width Height
  deriving (Show)

data Rectangle = Rec Point Width Height
  deriving (Show,Eq)

sortWidthFirst :: Rectangle -> Rectangle -> Ordering
sortWidthFirst (Rec _ w1 h1) (Rec _ w2 h2)
  | w1 < w2 = LT
  | w1 > w2 = GT
  | otherwise = compare h1 h2

sortHeightFirst :: Rectangle -> Rectangle -> Ordering
sortHeightFirst (Rec _ w1 h1) (Rec _ w2 h2)
  | h1 < h2 = LT
  | h2 > h1 = GT
  | otherwise = compare w1 w2

sortByArea :: Rectangle -> Rectangle -> Ordering
sortByArea (Rec _ w1 h1) (Rec _ w2 h2)
  | (w1 * h1) < (w2 * h2) = LT
  | (w1 * h1) > (w2 * h2) = GT
  | otherwise = EQ

getWidth :: Rectangle -> Width
getWidth (Rec _ w _)= w

getHeight :: Rectangle -> Height
getHeight (Rec _ _ h) = h

getMinX :: [Point] -> Double
getMinX pts = getMinX' pts (fromInteger (toInteger (maxBound :: Int)) :: Double)
    where getMinX' (p:ps) currMin | fst p < currMin = getMinX' ps (fst p)
                                  | otherwise       = getMinX' ps currMin
          getMinX' [] currMin     = currMin

getMaxX :: [Point] -> Double
getMaxX pts = getMaxX' pts (fromInteger (toInteger (minBound :: Int)) :: Double)
    where getMaxX' (p:ps) currMax | fst p > currMax = getMaxX' ps (fst p)
                                  | otherwise       = getMaxX' ps currMax
          getMaxX' [] currMax     = currMax
  
getMinY :: [Point] -> Double
getMinY pts = getMinY' pts (fromInteger (toInteger (maxBound :: Int)) :: Double)
    where getMinY' (p:ps) currMin | snd p < currMin = getMinY' ps (snd p)
                                  | otherwise       = getMinY' ps currMin
          getMinY' [] currMin     = currMin

getMaxY :: [Point] -> Double
getMaxY pts = getMaxY' pts (fromInteger (toInteger (minBound :: Int)) :: Double)
    where getMaxY' (p:ps) currMax | snd p > currMax = getMaxY' ps (snd p)
                                  | otherwise       = getMaxY' ps currMax
          getMaxY' [] currMax     = currMax

totalWaste :: Result -> Shape -> Double
totalWaste (R bs) (Stock w h) = foldr (\b acc -> acc + waste b w h) 0 bs 
totalWaste _ _ = 0

avgWastePerBin :: Result -> Shape -> Double
avgWastePerBin r@(R bs) s = totalWaste r s / fromIntegral (length bs)

waste :: Bin -> Width -> Height -> Double
waste (B rs) w h = w * h - foldr (
                            \r acc -> acc + getWidth r * getHeight r) 0 rs