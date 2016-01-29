{-# OPTIONS -fno-warn-type-defaults -fwarn-incomplete-patterns -Wall -fwarn-tabs -fno-warn-type-defaults #-}
module BinPack.MaxRect where

import BinPack
import Test.HUnit 
import Data.List as List (delete)

data FreeRect = FR Rectangle
   deriving (Show,Eq)

-- Functions for running maxrect algorithms
runMaxRect :: [Rectangle] -> ([FreeRect] -> Width -> Height -> Double -> Double -> [(Rectangle,Double,Double,Rectangle)] -> [(Rectangle,Double,Double,Rectangle)]) -> Shape -> Result
runMaxRect [] _ _                    = R []
runMaxRect rects func st@(Stock w h)= runAlg rects func (R []) st [FR (Rec (0,0) w h)]
runMaxRect _ _ _                     = R []

runAlg :: [Rectangle] -> ([FreeRect] -> Width -> Height -> Double -> Double -> [(Rectangle, Double, Double,Rectangle)] -> [(Rectangle, Double, Double,Rectangle)]) -> Result -> Shape -> [FreeRect] -> Result
runAlg [] _ (R bl) _ _                       = R bl
runAlg rect f res@(R bl) st@(Stock w' h') fr | null rt   = runAlg rect f (R (bl++[B []])) st [FR (Rec (0,0) w' h')]
                                             | otherwise =  runAlg (List.delete (last rfit) rect) f (appendToResult [last rt] res) st (splitFreeRects fr (last rt))
                                            where maxa          = fromInteger (toInteger (maxBound :: Int)) :: Double
                                                  (rt,_,_,rfit) = fitRectangles rect fr maxa maxa f 
runAlg _ _ _ _ _                              = R []

fitRectangles :: [Rectangle] -> [FreeRect] -> Double -> Double -> ([FreeRect] -> Width -> Height -> Double -> Double -> [(Rectangle,Double,Double,Rectangle)] -> [(Rectangle,Double,Double,Rectangle)]) -> ([Rectangle],Double,Double,[Rectangle])
fitRectangles rects' fr' ba' bss' func = fitRect rects' fr' ba' bss' func [] []
     where fitRect (r:rs) fr ba bss f bestFitRects rects = case f fr (getWidth r) (getHeight r) ba bss [] of
                                                               []       -> (bestFitRects , ba , bss,rects)
                                                               bestfits -> if ba'' < ba || (ba'' == ba && bss'' < bss)
                                                                   then fitRect rs fr ba'' bss'' f (bestFitRects ++ [r']) (rects ++ [rfit])
                                                                   else fitRect rs fr ba bss f bestFitRects rects
                                                                            where (r',bss'',ba'',rfit) = last bestfits
           fitRect [] _ ba bss _ bfr rfits               = (bfr,ba,bss,rfits)

-- Best short side fit algorithm
bestShortSideFit :: [FreeRect] -> Width -> Height -> Double -> Double -> [(Rectangle,Double,Double,Rectangle)] -> [(Rectangle,Double,Double,Rectangle)]
bestShortSideFit (fr:frs) w h bss bls rects = case getBestShortSideFit fr w h bss bls  of
                                               Left _                  -> bestShortSideFit frs w h bss bls rects
                                               Right a@(_,bss',bls',_) -> bestShortSideFit frs w h bss' bls' (rects ++ [a])
bestShortSideFit [] _ _ _ _ rects           = rects

getBestShortSideFit :: FreeRect -> Width -> Height -> Double -> Double -> Either (Maybe Double) (Rectangle,Double,Double,Rectangle)
getBestShortSideFit fr w h bss bls =  case getBestSSRectAtWidth fr w h bss bls of
                                     Just (r,ssf,lsf) -> case getBestSSRectAtHeight fr w h ssf lsf of
                                                           Just (r',s,l) -> Right (r',s,l,Rec (0,0) w h)
                                                           Nothing       -> Right (r,ssf,lsf,Rec (0,0) w h)
                                     Nothing          -> case getBestSSRectAtHeight fr w h bss bls of
                                                           Just (r',s,l) -> Right (r',s,l,Rec (0,0) w h)
                                                           Nothing       -> Left Nothing

getBestSSRectAtWidth :: FreeRect -> Width -> Height -> Double -> Double -> Maybe (Rectangle,Double,Double)
getBestSSRectAtWidth (FR (Rec (fx,fy) fw fh)) w h bss blf |(fw >= w && fh >= h)
                                                           && (ss < bss || (ss == bss && ls < blf))
                                                                      = Just (Rec (fx,fy) w h,ss,ls) 
                                                          | otherwise = Nothing
     where ss = min (abs (fw - w)) (abs (fh -h))
           ls = max (abs (fw - w)) (abs (fh -h))
 
getBestSSRectAtHeight :: FreeRect -> Width -> Height -> Double -> Double -> Maybe (Rectangle,Double,Double)
getBestSSRectAtHeight (FR (Rec (fx,fy) fw fh)) w h bss blf | (fw >= h && fh >= w)
                                                              && (ss < bss || (ss == bss && ls < blf))
                                                                       = Just (Rec (fx,fy) h w,ss ,ls)
                                                           | otherwise = Nothing
     where ss = min (abs (fw - h)) (abs (fh -w))
           ls = max (abs (fw - h)) (abs (fh -w))


--Best Area fit heuristic for determining fit position
bestAreaFit :: [FreeRect] -> Width -> Height -> Double -> Double -> [(Rectangle,Double,Double,Rectangle)] -> [(Rectangle,Double,Double,Rectangle)]
bestAreaFit (fr:frs) w h ba bss rects = case getBestAreaFit fr w h ba bss (area fr - (w * h)) of
                                           Left _                 -> bestAreaFit frs w h ba bss rects
                                           Right a@(_,bss',ba',_) -> bestAreaFit frs w h ba' bss' (rects ++ [a])

bestAreaFit [] _ _ _ _ rects          = rects

getBestAreaFit :: FreeRect -> Width -> Height -> Double -> Double -> Double -> Either (Maybe Double) (Rectangle,Double,Double,Rectangle)
getBestAreaFit fr w h ba bss ar =  case getBestRectAtWidth fr w h ba bss ar of
                                     Just (r,ssf,ar') -> case getBestRectAtHeight fr w h ar' ssf ar of
                                                           Just (r',s,a) -> Right (r',s,a,Rec (0,0) w h)
                                                           Nothing       -> Right (r,ssf,ar',Rec (0,0) w h)
                                     Nothing          -> case getBestRectAtHeight fr w h ba bss ar of
                                                           Just (r',s,a) -> Right (r',s,a,Rec (0,0) w h)
                                                           Nothing       -> Left Nothing

getBestRectAtWidth :: FreeRect -> Width -> Height -> Double -> Double -> Double -> Maybe (Rectangle,Double,Double)
getBestRectAtWidth (FR (Rec (fx,fy) fw fh)) w h ba bssf ar |(fw >= w && fh >= h) 
                                                             && (ar < ba || (ar == ba && ss < bssf))  
                                                                         = Just (Rec (fx,fy) w h,bssf , ar) 
                                                           | otherwise   = Nothing
     where ss = min (abs (fw - w)) (abs (fh -h))
 
getBestRectAtHeight :: FreeRect -> Width -> Height -> Double -> Double -> Double -> Maybe (Rectangle,Double,Double)
getBestRectAtHeight (FR (Rec (fx,fy) fw fh)) w h ba bssf ar |(fw >= h && fh >= w) 
                                                              && (ar < ba || (ar == ba && ss < bssf)) 
                                                                         = Just (Rec (fx,fy) h w,bssf , ar)
                                                            | otherwise  = Nothing
     where ss = min (abs (fw - h)) (abs (fh -w))

-- Common function for splitting the freerects based on the rectangle that it fit
splitFreeRects :: [FreeRect] -> Rectangle -> [FreeRect]
splitFreeRects (fr:frs) r = splitFreeRect fr r ++ splitFreeRects frs r
splitFreeRects [] _       = []

splitFreeRect :: FreeRect -> Rectangle -> [FreeRect]
splitFreeRect f@(FR (Rec (fx,fy) fw fh)) r@(Rec (rx,ry) rw rh) | rx >= fx +fw
                                                                 || rx + rw <= fx 
                                                                 || ry >= fy + fh
                                                                 || ry + rh <= fy = [f]
                                                               | otherwise        = splitAtWidth f r ++ splitAtHeight f r
splitAtWidth :: FreeRect -> Rectangle -> [FreeRect]
splitAtWidth f@(FR (Rec (fx,_) fw _)) r@(Rec (rx,_) rw _) | rx < fx + fw 
                                                            && rx + rw > fx 
                                                                      = splitWidthAtTop f r ++ splitWidthAtBottom f r
                                                          | otherwise = [] 
splitWidthAtTop :: FreeRect -> Rectangle -> [FreeRect]
splitWidthAtTop (FR (Rec (fx,fy) fw fh)) (Rec (_,ry) _ _) | ry > fy 
                                                            && ry < fy + fh 
                                                                      = [FR (Rec (fx,fy) fw (ry - fy))]
                                                          | otherwise = []
splitWidthAtBottom :: FreeRect -> Rectangle -> [FreeRect]
splitWidthAtBottom (FR (Rec (fx,fy) fw fh)) (Rec (_,ry) _ rh) | ry + rh < fy + fh = [FR (Rec (fx , ry + rh) fw (fy + fh - (ry + rh)))]
                                                              | otherwise         = []
splitAtHeight :: FreeRect -> Rectangle -> [FreeRect]
splitAtHeight f@(FR (Rec (_,fy) _ fh)) r@(Rec (_,ry) _ rh) | ry < fy + fh 
                                                             && ry + rh > fy 
                                                                       = splitHeightAtLeft f r ++ splitHeightAtRight f r
                                                           | otherwise = []
splitHeightAtLeft :: FreeRect -> Rectangle -> [FreeRect]
splitHeightAtLeft (FR (Rec (fx,fy) fw fh)) (Rec (rx,_) _ _) | rx > fx 
                                                              && rx < fx + fw 
                                                                        = [FR (Rec (fx,fy) (rx - fx) fh)]
                                                            | otherwise = []

splitHeightAtRight :: FreeRect -> Rectangle -> [FreeRect]
splitHeightAtRight (FR (Rec (fx,fy) fw fh)) (Rec (rx,_) rw _) | rx + rw < fx + fw = [FR (Rec (rx + rw,fy) (fx + fw - (rx + rw)) fh)]
                                                              | otherwise         = []

-- Function for deleting duplicate (Overlapping) free rectangles
removeDuplicates :: [FreeRect] -> [FreeRect]
removeDuplicates rects = deleteAll rects $ removedups rects rects

deleteAll :: [FreeRect] -> [FreeRect] -> [FreeRect]
deleteAll = foldl (flip List.delete)

removedups :: [FreeRect] -> [FreeRect] -> [FreeRect]
removedups (r:rs) rects = case remove r (List.delete r rects) of
                           Just a  -> a : removedups rs rects  
                           Nothing -> removedups rs rects
removedups [] _         = []

remove :: FreeRect -> [FreeRect] -> Maybe FreeRect
remove r1 (r1':rs') = if r1' `contains` r1 then Just r1' else remove r1 rs'
remove _ []        = Nothing

contains :: FreeRect -> FreeRect -> Bool
contains (FR (Rec (p1x,p1y) w1 h1)) (FR (Rec (p2x,p2y) w2 h2)) = p1x >= p2x 
                                                                 && p1y >= p2y 
                                                                 && p1x + w1 <= p2x + w2 
                                                                 && p1y + h1 <= p2y + h2

--Functions for handling result operations
replaceBin :: Result -> Bin -> Bin -> Result
replaceBin (R bl) orig new = R (List.delete orig bl ++ [new] ) 

appendToResult :: [Rectangle] -> Result -> Result
appendToResult r (R [])     = R [B r]
appendToResult r res@(R bl) = replaceBin res (last bl) $ appendToBin r (last bl)

appendToBin :: [Rectangle] -> Bin -> Bin
appendToBin r (B rl) = B (rl ++ r)

-- Generic functions
area :: FreeRect -> Double
area (FR (Rec (_,_) fw fh)) = fw * fh

getRectangle :: FreeRect -> Rectangle
getRectangle (FR r) = r

--Tests for most important functions
testContains :: Test
testContains = "testContains" ~: 
               TestList [
                          contains (FR (Rec (2,3) 1 1)) (FR (Rec (1,2) 3 4)) ~?= True,
                          contains (FR (Rec (0,0) 0 0)) (FR (Rec (1,1) 1 1)) ~?= False
                         ]

testRemoveDuplicates :: Test
testRemoveDuplicates = "testDuplicateRemoval" ~: 
                       TestList [
                                  removeDuplicates [FR (Rec (2,3) 1 1), FR (Rec (1,2) 3 4)] ~?= [FR (Rec (1.0,2.0) 3.0 4.0)],
                                  removeDuplicates [FR (Rec (0,0) 0 0), FR (Rec (1,1) 1 1)] ~?= [FR (Rec (0.0,0.0) 0.0 0.0),FR (Rec (1.0,1.0) 1.0 1.0)]
                                ]

testSplitRects :: Test
testSplitRects = "testSplitRects" ~:
                 TestList [
                             splitFreeRects [FR (Rec (2,3) 1 1), FR (Rec (1,2) 3 4)] (Rec (2,3) 0.25 0.25) ~?= [FR (Rec (2.0,3.25) 1.0 0.75),FR (Rec (2.25,3.0) 0.75 1.0),FR (Rec (1.0,2.0) 3.0 1.0),FR (Rec (1.0,3.25) 3.0 2.75),FR (Rec (1.0,2.0) 1.0 4.0),FR (Rec (2.25,2.0) 1.75 4.0)],
                             splitFreeRects  [FR (Rec (0,0) 4 4)] (Rec (0.0,0.0) 2.0 2.0) ~?= [FR (Rec (0.0,2.0) 4.0 2.0),FR (Rec (2.0,0.0) 2.0 4.0)],
                             splitFreeRects [FR (Rec (2,3) 1 1), FR (Rec (1,2) 3 4)] (Rec (2.0,3.0) 0.25 0.25) ~?= [FR (Rec (2.0,3.25) 1.0 0.75),FR (Rec (2.25,3.0) 0.75 1.0),FR (Rec (1.0,2.0) 3.0 1.0),FR (Rec (1.0,3.25) 3.0 2.75),FR (Rec (1.0,2.0) 1.0 4.0),FR (Rec (2.25,2.0) 1.75 4.0)]
                          ]
testMaxRect :: Test
testMaxRect = "testMaxRect" ~: 
              TestList [
                       runMaxRect [Rec (0,0) 3 2,Rec (0,0) 4 2,Rec (0,0) 4 2,Rec (0,0) 4 2] bestAreaFit (Stock 4 4) ~?= R [B [Rec (0.0,0.0) 2.0 4.0,Rec (2.0,0.0) 2.0 4.0],B [Rec (0.0,0.0) 2.0 4.0,Rec (2.0,0.0) 2.0 3.0]],
                       runMaxRect [Rec (0,0) 3 2,Rec (0,0) 4 2,Rec (0,0) 4 2,Rec (0,0) 4 2,Rec (0,0) 3 4] bestAreaFit (Stock 4 4) ~?= R [B [Rec (0.0,0.0) 4.0 3.0],B [Rec (0.0,0.0) 2.0 4.0,Rec (2.0,0.0) 2.0 4.0],B [Rec (0.0,0.0) 2.0 4.0,Rec (2.0,0.0) 2.0 3.0]],
                       runMaxRect [Rec (0.0,0.0) 6.7 3.5,Rec (0,0) 2.3 1.6] bestShortSideFit (Stock 10 10) ~?= R [B [Rec (0.0,0.0) 6.7 3.5,Rec (6.7,0.0) 2.3 1.6]],
                       runMaxRect [Rec (0.0,0.0) 6.7 3.5,Rec (0.0,0.0) 6.7 3.5,Rec (0,0) 2.3 1.6] bestShortSideFit (Stock 10 10) ~?= R [B [Rec (0.0,0.0) 6.7 3.5,Rec (0.0,3.5) 6.7 3.5,Rec (0.0,7.0) 1.6 2.3]]
                       ]