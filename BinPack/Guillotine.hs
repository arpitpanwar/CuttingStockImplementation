{-# OPTIONS -fno-warn-type-defaults -fwarn-incomplete-patterns -Wall -fwarn-tabs -fno-warn-type-defaults #-}
module BinPack.Guillotine where
import BinPack
import Data.List
import Test.HUnit

--Origin is left and down-most point for rec
data Cut = Cut Rectangle
  deriving (Show,Eq)

data Option = Axis | Leftover
  deriving (Show, Eq)

--For use with test cases below
recB :: Rectangle
recB = Rec (0, 0) 2 2

recA :: Rectangle
recA = Rec (4, 3) 5 10

recC :: Rectangle
recC = Rec (0, 0) 2 2

recD :: Rectangle
recD = Rec (0, 2) 4 4

recE :: Rectangle
recE = Rec (2, 0) 2 1

recF :: Rectangle
recF = Rec (2, 1) 2 1

recBox :: [Rectangle]
recBox = [recC, recD, recE, recF]

--Shape is stock piece
guillotine :: [Rectangle] -> Shape -> Option -> Result
guillotine (p:pcs) (Stock sw sh) opt = R(new_bin : next_bin)
  where
  R next_bin = guillotine rem_cuts (Stock sw sh) opt
  rem_cuts_and_bin = fillStock (p:pcs) [Rec (0,0) sw sh] (B []) opt
  rem_cuts = fst rem_cuts_and_bin
  new_bin = snd rem_cuts_and_bin

guillotine [] _ _ = R [ ]
guillotine _ (Polygon _) _ = R [B [] ]


--Takes in list of rectangles to cut and a stock shape put in the bin
--and returns the filled bin with the remaining rectangles
fillStock :: [Rectangle] -> [Rectangle] -> Bin -> Option -> ([Rectangle], Bin)
fillStock (c:cs) freeRecs (B bin) opt =
    case bestRec of
      Just x -> fillStock cs (newFreeRecs x) (newBin x) opt
      Nothing -> (c:cs, B bin)
    where
    bestRec = fst (chooseRecBestArea freeRecs c Nothing)
    rotate = snd (chooseRecBestArea freeRecs c Nothing)
    removeFreeRec x = delete x freeRecs
    oneCut x = if rotate then makeCut (transRec c) x opt else makeCut c x opt
    newFreeRecs x =
      removeFreeRec x ++ [getFirst $ oneCut x, getSecond (oneCut x)]
    ct x = getThird (oneCut x)
    newBin x = B (bin ++ [getRectangle (ct x)])

fillStock [] _ (B bin) _ = ([], B bin)

testFillStock :: Test
testFillStock = "testFillStock" ~:
  TestList [
            fillStock recBox [Rec (0,0) 6 4] (B []) Axis ~?=
              ([],B [Rec (0.0,0.0) 2.0 2.0,Rec (2.0,0.0) 4.0 4.0,Rec (0.0,2.0) 2.0 1.0,Rec (0.0,3.0) 2.0 1.0])
          ]


getFirst :: (b,b,a) -> b
getFirst (b,_,_) = b

getSecond :: (b,b,a) -> b
getSecond (_,b,_) = b

getThird :: (b,b,a) -> a
getThird (_,_,a) = a

getRectangle :: Cut -> Rectangle
getRectangle (Cut r) = r

--Takes in a rec and a cut rec and sees if they fit
checkFit :: Rectangle -> Rectangle -> Bool
checkFit (Rec _ sw sh) (Rec _ cw ch) = (sw >= cw) && (sh >= ch)

--Takes in a rec and a cut rec and sees if the cut rec in transpose fits
checkTransFit :: Rectangle -> Rectangle -> Bool
checkTransFit stock cut = checkFit stock (transRec cut)

--Takes in a a cut and smallest so far and determines if the 1st is smaller
smaller :: Rectangle -> Rectangle -> Bool
smaller (Rec _ pw ph) (Rec _ smallw smallh) = smallw * smallh > pw * ph

--Choose rec that picks the rec with smallest area that fits and returns that rectangle
chooseRecBestArea :: [Rectangle] -> Rectangle -> Maybe Rectangle -> (Maybe Rectangle, Bool)
chooseRecBestArea [] cut (Just result)
  | checkFit result cut = (Just result, False)
  | checkTransFit result cut = (Just result, True)
  | otherwise = (Nothing, False) --this would be an error

chooseRecBestArea [] _ Nothing = (Nothing, False)

chooseRecBestArea (pot:cs) cut (Just small)
  | checkFit pot cut && smaller pot small = chooseRecBestArea cs cut (Just pot)
  | checkTransFit pot cut && smaller pot small = chooseRecBestArea cs cut (Just pot)
  | otherwise =  chooseRecBestArea cs cut (Just small)

chooseRecBestArea (pot:cs) cut Nothing
  | checkFit pot cut = chooseRecBestArea cs cut (Just pot)
  | checkTransFit pot cut = chooseRecBestArea cs cut (Just $ transRec pot)
  | otherwise = chooseRecBestArea cs cut Nothing


testChooseRecBestArea :: Test
testChooseRecBestArea = "testChooseRecBestArea" ~:
  TestList [
            chooseRecBestArea recBox recB Nothing ~?= (Just (Rec (0.0,0.0) 2.0 2.0),False),
            chooseRecBestArea recBox recA Nothing ~?= (Nothing, False),
            chooseRecBestArea recBox (Rec (0, 0) 1 2) Nothing ~?= (Just recE, True), --It is flipping it
            chooseRecBestArea recBox (Rec (0, 0) 3 4) Nothing ~?= (Just recD, False)
          ]

--Rectangle Piece, Retangle stock -> (cut,
makeCut :: Rectangle -> Rectangle -> Option -> (Rectangle, Rectangle, Cut)
makeCut (Rec _ pw ph) (Rec o@(ox, oy) sw sh) opt =
  if split then (horSubShape, horSubShape', cut) else (verSubShape, verSubShape', cut)
  where
    cut = Cut (Rec o pw ph)
    horSubShape = Rec (ox, oy + ph) sw (sh - ph)
    horSubShape' = Rec (ox + pw, oy) (sw - pw) ph
    verSubShape = Rec (ox, oy + ph) pw (sh - ph)
    verSubShape' = Rec (ox + pw, oy) (sw - pw) sh
    split = case opt of
              Axis -> longerAxisSplit sw sh
              Leftover -> longerLeftoverSplit (Rec o sw sh) (Rec (0, 0) pw ph)

transRec :: Rectangle -> Rectangle
transRec (Rec o w h) = Rec o h w

testMakeCut :: Test
testMakeCut = "testMakeCut" ~:
              TestList [
                        makeCut recB recA Axis ~?=
                          (Rec (4,5) 5 8, Rec (6, 3) 3 2, Cut (Rec (4, 3) 2 2)),  --Horizontal cut
                        makeCut recB (transRec recA) Axis ~?=
                          (Rec (4.0,5.0) 2.0 3.0, Rec (6, 3) 8 5, Cut (Rec (4,3) 2 2)), --Vertical Cut
                        makeCut (Rec (0, 0) 2 1) (Rec (0, 0) 3 3) Leftover ~?=
                          (Rec (0.0,1.0) 3.0 2.0,Rec (2.0,0.0) 1.0 1.0,Cut (Rec (0.0,0.0) 2.0 1.0)), --Horizontal cut Leftover
                        makeCut (Rec (0, 0) 1 2) (Rec (0, 0) 3 3) Leftover ~?=
                        (Rec (0.0,2.0) 1.0 1.0,Rec (1.0,0.0) 2.0 3.0,Cut (Rec (0.0,0.0) 1.0 2.0)) --Vertical cut Leftover
              ]

longerAxisSplit :: Double -> Double -> Bool
longerAxisSplit w h = h > w

--takes in stock rec and piece/cut rect and determines which split to use
longerLeftoverSplit :: Rectangle -> Rectangle -> Bool
longerLeftoverSplit (Rec _ sw sh) (Rec _ pw ph) = sw - pw < sh - ph
