{-# OPTIONS -fwarn-incomplete-patterns -Wall -fwarn-tabs -fno-warn-type-defaults -fwarn-unused-matches #-}
module BinPack.Shelf where

import BinPack
import Test.HUnit
import Control.Applicative

type Acc = Int

type Index = Int

type Fill = Double

type Dim = (Width, Height, Fill) 

type Piece = (Width, Height)

-- | Internal Result and Bin that are converted into output Result and Bin

data IResult = Ri Dim [IBin] 
  deriving (Show, Eq)

data IBin = Bi Dim [Shelf]
  deriving (Show, Eq)

data Shelf = S Dim [Piece]
  deriving (Show, Eq)

-- |
-- | Implementation of the Shelf Next Fit heuristic
-- |

shelfNextFit :: [Rectangle] -> Shape -> Result
shelfNextFit r s  = R $ convertB bins
                    where (Ri _ bins) = sNF (pieces r) $ emptyResult s

sNF :: [Piece] -> IResult -> IResult
sNF [] r       = r
sNF p@(x:xs) r = case addToBin x $ curBin r of
                      Just b  -> sNF xs $ updateResult r b
                      Nothing -> sNF p $ addBin r

addToBin :: Piece -> IBin -> Maybe IBin
addToBin p b@(Bi _ [])         = addShelf p b
addToBin p@(_, _) b@(Bi _ _) = case addToShelf p $ curShelf b of
                                   Just s  -> Just $ updateBin b s
                                   Nothing -> addShelf p b                      
-- | 
-- | Implementation of the Shelf Best Area Fit heuristic
-- | 

shelfBestAreaFit :: [Rectangle] -> Shape -> Result
shelfBestAreaFit r s = R $ convertB bins
                       where (Ri _ bins) = sBAF (pieces r) $ emptyResult s

sBAF :: [Piece] -> IResult -> IResult
sBAF [] r       = r
sBAF p@(x:xs) r = case bestShelfBin x $ curBin r of
                      Just b  -> sBAF xs $ updateResult r b
                      Nothing -> sBAF p $ addBin r

-- | Attach the given index to the given value in a monad

withIndex :: (Monad m) => Index -> m a -> m (Index, a)
withIndex i m = do v <- m
                   return (i, v)

-- | Given a piece and a bin, returns the best bin that can be created by adding the piece

bestShelfBin :: Piece -> IBin -> Maybe IBin
bestShelfBin p b@(Bi d ss) = case bestFit of
                                Nothing -> addShelf p b
                                Just (i, bs) -> Just (Bi d $ replaceShelfAt bs ss i 0)
                             where shelves = fitPieceInShelves p ss
                                   bestFit = bestShelf shelves Nothing 0

-- | Replaces the given shelf in the given list of shelves

replaceShelfAt :: Shelf -> [Shelf] -> Index -> Acc -> [Shelf]
replaceShelfAt _ [] _ _         = []
replaceShelfAt s (s':ss') i acc = if i == acc
                                   then s : ss'
                                   else s' : replaceShelfAt s ss' i (inc acc)

-- | Find the best shelf with the least remaining area

bestShelf :: [Maybe Shelf] -> Maybe (Index, Shelf) -> Index -> Maybe (Index, Shelf)
bestShelf [] acc _                      = acc
bestShelf (Nothing:ss) acc iacc         = bestShelf ss acc (inc iacc)
bestShelf (Just s:ss) Nothing iacc      = bestShelf ss (Just (iacc, s)) (inc iacc) 
bestShelf (Just s:ss) (Just (i,s')) iacc = if freeA s < freeA s' 
                                           then bestShelf ss (Just (iacc, s)) (inc iacc)
                                           else bestShelf ss (Just (i,s')) (inc iacc)


-- | Tries to place the given piece in all given shelves

fitPieceInShelves :: Piece -> [Shelf] -> [Maybe Shelf]
fitPieceInShelves _ []     = []
fitPieceInShelves p (s:ss) = s' : fitPieceInShelves p ss
                             where s' = addToShelf p s

-- |
-- | Converters from the inner Shelf types to the output Results
-- |

convertB :: [IBin] -> [Bin]
convertB [] = []
convertB (Bi _ ss:bs) = B (convertS ss 0) : convertB bs

convertS :: [Shelf] -> Height -> [Rectangle]
convertS [] _ = []
convertS (S d ps:ss) h = convertP ps 0 h ++ convertS ss (h + height d)

convertP :: [Piece] -> Width -> Height -> [Rectangle]
convertP [] _ _ = []
convertP ((pw, ph):pp) w h  = Rec (w, h) pw ph : convertP pp (w + pw) h  

pieces :: [Rectangle] -> [Piece]
pieces = map (\(Rec _ w h) -> (w, h))

-- |
-- | Core helper functions that are shared between implementations
-- |

-- | Tries to create a new Shelf from the given Piece

newShelf :: Piece -> IBin -> Maybe Shelf
newShelf p@(w, h) (Bi d _)
  | h + fill d > height d = Nothing
  | w > width d = Nothing
  | otherwise = Just $ S (width d, h, w) [p]

-- | Tries to a new shelf with the given piece to a bin

addShelf :: Piece -> IBin -> Maybe IBin
addShelf p b@(Bi d ss)  = do s@(S sd _) <- newShelf p b <|>
                                           newShelf (rotate p) b
                             return $ Bi (incFill d $ height sd) (s:ss)  

-- | Tries to add the given Piece to the given Shelf

addToShelf :: Piece -> Shelf -> Maybe Shelf
addToShelf p (S d ps) = if pw + fill d <= width d
                         then Just $ S (incFill d pw) (np:ps)
                         else Nothing
                       where np@(pw, _) = optimize p $ height d

-- | Optimize the placing of a piece given the Height of Shelf   

optimize :: Piece -> Height -> Piece
optimize p@(pw, ph) h = if (pw > ph && pw <= h) ||
                            (ph > pw && ph > h)
                          then rotate p
                          else p
-- |
-- | Trivial helper functions that are shared between implementations
-- |

freeA :: Shelf -> Double
freeA (S d _) = (width d - fill d) * height d 

inc :: Int -> Int
inc = (+) 1

maxD :: Piece -> Double
maxD (h, w) = if h > w then h else w

minD :: Piece -> Double
minD (h, w) = if h < w then h else w

rotate :: Piece -> Piece
rotate (w, h) = (h, w)

width :: Dim -> Width
width (w, _, _) = w 

height :: Dim -> Height
height (_, h, _) = h 

fill :: Dim -> Fill
fill (_, _, f) = f

emptyResult :: Shape -> IResult
emptyResult (Stock w h) = Ri d [emptyBin d]
                          where d = (w, h, 0) 
emptyResult (Polygon _ ) = undefined --remove 

emptyBin :: Dim -> IBin
emptyBin d = Bi d []

curBin :: IResult -> IBin
curBin (Ri _ (b:_)) = b
curBin _ = error "result is empty"

curShelf :: IBin -> Shelf
curShelf (Bi _ (s:_)) = s
curShelf _ = error "bin is empty"

addBin :: IResult -> IResult
addBin (Ri d bs) = Ri d $ emptyBin d : bs

updateResult :: IResult -> IBin -> IResult
updateResult (Ri d (_:bs)) b = Ri d (b:bs)
updateResult _ _ = error "result is empty"

updateBin :: IBin -> Shelf -> IBin
updateBin (Bi d (_:ss)) s = Bi d (s:ss)
updateBin _ _ = error "bin is empty"

updateShelf :: Shelf -> Piece -> Shelf
updateShelf (S d (_:ss)) s = S d (s:ss)
updateShelf _ _ = error "shelf is empty"

incFill :: Dim -> Fill -> Dim
incFill (w, h, f) nf = (w, h, f + nf)

area :: Piece -> Double
area (pw, pd) = pw * pd

-- |
-- | Tests for the Shelf Next Fit implementation
-- |

newShelfT :: Piece -> Width -> Shelf
newShelfT p@(w, h) bw = if w > bw
                         then S (bw, h, w) [p]
                         else S (bw, w, h) [rotate p]

binWithFullShelf :: IBin
binWithFullShelf = Bi (10, 10, 5) [S (10, 5, 10) [(6, 1), (4, 5)]]

binWithHalfShelf :: IBin
binWithHalfShelf = Bi (10, 10, 5) [S (10, 5, 4) [(4, 5)]]

testSNF :: Test
testSNF = "testSNF" ~:
   TestList [ testShelfNextFit , testAddToBin, testAddToShelf ]

testShelfNextFit :: Test
testShelfNextFit = "shelfNextFit" ~:
      TestList [
        shelfNextFit [Rec (0, 0) 5 5, Rec (0, 0) 5 5, Rec (0, 0) 5 5] (Stock 5 5) ~?= 
          R [B [Rec (0,0) 5 5], B [Rec (0,0) 5 5], B [Rec (0,0) 5 5]],
        shelfNextFit [Rec (0, 0) 5 5, Rec (0, 0) 5 5, Rec (0, 0) 5 5] (Stock 10 5) ~?= 
          R [B [Rec (0,0) 5 5], B [Rec (0,0) 5 5, Rec (5,0) 5 5]],
        shelfNextFit [Rec (0, 0) 5 5, Rec (0, 0) 5 5, Rec (0, 0) 5 5] (Stock 5 10) ~?= 
          R [B [Rec (0,0) 5 5], B [Rec (0,0) 5 5, Rec (0,5) 5 5]],
        shelfNextFit [Rec (0, 0) 5 5, Rec (0, 0) 5 5, Rec (0, 0) 5 5, Rec (0, 0) 5 5] (Stock 10 10) ~?= 
          R [B [Rec (0,0) 5 5, Rec (5,0) 5 5, Rec (0,5) 5 5, Rec (5,5) 5 5]]
      ]

testAddToBin :: Test
testAddToBin = "addtoBin" ~:
      TestList [
                 addToBin (5, 5) (emptyBin (10, 10, 0)) ~?= Just (Bi (10, 10, 5) [S (10, 5, 5) [(5, 5)]]),
                 addToBin (6, 1) binWithHalfShelf ~?= Just binWithFullShelf,
                 addToBin (1, 1) binWithFullShelf ~?= 
                    Just (Bi (10, 10, 6) [S (10, 1, 1) [(1, 1)], S (10, 5, 10) [(6, 1), (4, 5)]]),
                 addToBin (1, 10) binWithFullShelf ~?=
                    Just (Bi (10, 10, 6) [S (10, 1, 10) [(10, 1)], S (10, 5, 10) [(6, 1), (4, 5)]]),
                 addToBin (1, 11) (emptyBin (10, 11, 0)) ~?= Just (Bi (10, 11, 11) [S (10, 11, 1) [(1, 11)]]),
                 addToBin (6, 6) binWithFullShelf ~?= Nothing
               ]

testAddToShelf :: Test
testAddToShelf = "addToShelf" ~: 
      TestList [
                addToShelf (10, 10) (newShelfT (10, 10) 20) ~?= Just (S (20, 10, 20) [(10, 10), (10, 10)]),
                addToShelf (10, 9) (newShelfT (10, 10) 20) ~?= Just (S (20, 10, 19) [(9, 10), (10, 10)]),
                addToShelf (10, 11) (newShelfT (10, 10) 30) ~?= Just (S (30, 10, 21) [(11, 10), (10, 10)]),
                addToShelf (10, 11) (newShelfT (10, 10) 20) ~?= Nothing,
                addToShelf (11, 10) (newShelfT (10, 10) 20) ~?= Nothing,
                addToShelf (11, 11) (newShelfT (10, 10) 20) ~?= Nothing,
                addToShelf (1, 1) (S (3, 1, 2) [(1, 1), (1, 1)]) ~?= Just (S (3, 1, 3) [(1, 1), (1, 1), (1, 1)]),
                addToShelf (1, 1) (S (3, 1, 3) [(1, 1), (1, 1)]) ~?= Nothing
               ]

-- |
-- | Tests for the Shelf Best Area Fit implementation
-- |


binFullShelfs :: IBin
binFullShelfs = Bi (10, 20, 10) [full, full]

fullBin :: IBin
fullBin = Bi (10, 10, 10) [full, full]

halfFull :: Shelf
halfFull = S (10, 5, 5) [(5, 5)] 

fourthFull :: Shelf
fourthFull = S (10, 5, 2.5) [(2.5, 5)] 

full :: Shelf
full = S (10, 5, 10) [(5, 5), (5, 5)]

testBAF :: Test
testBAF = "testSNF" ~:
   TestList [ testBestShelfBin , testReplaceShelfAt, testBestShelf, testFitPieceInShelves ]

testBestShelfBin :: Test
testBestShelfBin = "bestShelfBin" ~: 
    TestList [
                bestShelfBin (5, 5) fullBin ~?= Nothing,
                bestShelfBin (5, 5) binFullShelfs ~?= Just (Bi (10, 20, 15) [halfFull, full, full]),
                bestShelfBin (5, 5) (Bi (10, 20, 0) []) ~?= Just (Bi (10, 20, 5) [halfFull]),
                bestShelfBin (5, 5) (Bi (10, 20, 10) [full, halfFull]) ~?= Just (Bi (10, 20, 10) [full, full]),
                bestShelfBin (5, 5) (Bi (10, 20, 10) [fourthFull, fourthFull, full, halfFull, fourthFull]) ~?= 
                                  Just (Bi (10, 20, 10) [fourthFull, fourthFull, full, full, fourthFull])
             ]

testReplaceShelfAt :: Test
testReplaceShelfAt = "replaceShelfAt" ~: 
    TestList [
               replaceShelfAt halfFull [full, full, full] 3 0 ~?= [full, full, full],
               replaceShelfAt halfFull [full, full, full] 2 0 ~?= [full, full, halfFull],
               replaceShelfAt halfFull [full, full, full] 0 0 ~?= [halfFull, full, full],
               replaceShelfAt halfFull [full] 0 0 ~?= [halfFull]
             ]

testBestShelf :: Test
testBestShelf = "bestShelf" ~: 
    TestList [
               bestShelf [Nothing, Nothing] Nothing 0 ~?= Nothing,
               bestShelf [Nothing, Nothing, Just halfFull] Nothing 0 ~?= 
                  Just (2, halfFull),
               bestShelf [Just halfFull, Nothing, Nothing] Nothing 0 ~?= 
                  Just (0, halfFull),
               bestShelf [Just halfFull, Nothing, Just halfFull] Nothing 0 ~?= 
                  Just (0, halfFull),
               bestShelf [Just fourthFull, Nothing, Just fourthFull, Just halfFull] Nothing 0 ~?= 
                  Just (3, halfFull),
               bestShelf [Nothing, Just halfFull, Nothing, Just fourthFull] Nothing 0 ~?= 
                  Just (1, halfFull),
               bestShelf [Just halfFull, Nothing, Nothing, Just fourthFull] Nothing 0 ~?= 
                  Just (0, halfFull)
             ]

testFitPieceInShelves :: Test
testFitPieceInShelves = "fitPieceInShelves" ~: 
    TestList [
               fitPieceInShelves (5, 5) [halfFull] ~?= [Just full],
               fitPieceInShelves (5, 6) [halfFull] ~?= [Nothing],
               fitPieceInShelves (6, 5) [halfFull, S (11, 5, 5) [(5, 5)], S (11, 5, 6) [(5, 5)] ] ~?= 
                                          [Nothing, Just $ S (11, 5, 11) [(6, 5), (5, 5)], Nothing]                             
             ]