{-# OPTIONS -fno-warn-type-defaults -fwarn-incomplete-patterns -Wall -fwarn-tabs -fno-warn-type-defaults #-}
import BinPack
import BinPack.Shelf
import BinPack.MaxRect
import BinPack.Guillotine
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import System.Environment
import System.IO
import Text.Read (readMaybe)

-- Main function which takes in a filename and runs the algorithm

main :: IO ()
main = do
         args <- getArgs
         case args of
          [x] -> putStrLn "Parsing input file " >> parseFile x
          _   -> putStrLn "Only one argument accepted which should be the file name of the shapes."


-- Reads the input file and gives the choice of algorithm

parseFile :: String -> IO()
parseFile f = do
               handle  <- openFile f ReadMode
               content <- hGetContents handle
               let parsed = parseLinesToDouble $ map words $ lines content in
                case parseStock $ head parsed of
                 Just s  -> chooseOrdering (getRectangles (tail parsed) s) s
                 _       -> fail "Invalid Stock"
               hClose handle

-- Gives user a choice of ordering and sorts the pieces by it

chooseOrdering :: [Rectangle] -> Shape -> IO ()
chooseOrdering rects st = do putStrLn orderChoice
                             ord <- getLine
                             case readMaybe ord of
                               Just x  -> do let ordered = orderRectangles x rects
                                             chooseAlgorithm ordered st
                               Nothing -> do putStrLn ("Invalid input: " ++ ord)
                                             chooseOrdering rects st

-- Gives user a choice of algorithm and executes that algorithm

chooseAlgorithm :: [Rectangle] -> Shape -> IO ()
chooseAlgorithm rects st = do putStrLn algChoice
                              alg <- getLine
                              case readMaybe alg of
                               Just x  -> chooseOutput (getAlgorithmOutput x rects st) st
                               Nothing -> do putStrLn ("Invalid input: " ++ alg)
                                             chooseAlgorithm rects st

-- Asks users what is the expected output format

chooseOutput :: Result -> Shape -> IO ()
chooseOutput r s = do putStrLn outputFormat
                      out <- getLine
                      case readMaybe out of
                          Just x -> generateOutput x r s
                          Nothing -> do putStrLn ("Invalid input: " ++ out)
                                        chooseOutput r s

-- Prints the results

printResults :: Result -> Shape -> IO ()
printResults r st = do putStr "Total waste: "
                       print (totalWaste r st)
                       putStr "\nAvg waste per bin: "  
                       print (avgWastePerBin r st)
                       putStr "\nResults:\n\n"
                       print r

-- Generates text output

makeTextOutput :: Result -> Shape -> String
makeTextOutput r s = concat ["Total waste: ", show $ totalWaste r s,
                            "\nAvg waste per bin: ", show $ avgWastePerBin r s,
                            "\nResults:\n\n", show r]   

-- Generate Elm source output

makeElmSource :: Result -> Shape -> IO ()
makeElmSource r s@(Stock w h) = do template <- readFile "./extra/template.elm"
                                   let s' = concat [
                                          template, 
                                          "\nbwidth = ", show w,
                                          "\nbheight = ", show h,
                                          "\nwaste = \"Total waste: ",
                                          show $ totalWaste r s,
                                          "\"\navg_waste = \"Avg waste: ",
                                          show $ avgWastePerBin r s,
                                          "\"\nresult = ", show r]   
                                   writeToFile s'
makeElmSource _ _ = return ()

-- Write results to file

writeToFile :: String -> IO ()
writeToFile s = do putStrLn "Enter the file path:"
                   path <- getLine
                   writeFile path s

-- Runs the specified algorithm based on user choice

getAlgorithmOutput :: Int -> [Rectangle] -> Shape -> Result
getAlgorithmOutput 1 rects st@(Stock _ _) = runMaxRect rects bestAreaFit st
getAlgorithmOutput 2 rects st@(Stock _ _) = runMaxRect rects bestShortSideFit st
getAlgorithmOutput 3 rects st@(Stock _ _) = guillotine rects st Axis
getAlgorithmOutput 4 rects st@(Stock _ _) = guillotine rects st Leftover
getAlgorithmOutput 5 rects st@(Stock _ _) = shelfBestAreaFit rects st
getAlgorithmOutput 6 rects st@(Stock _ _) = shelfNextFit rects st
getAlgorithmOutput _ _ _                  = R []

-- Sorts the input pieces based on user choice

orderRectangles :: Int -> [Rectangle] -> [Rectangle]
orderRectangles 2 rects = sortBy sortWidthFirst rects
orderRectangles 3 rects = sortBy (flip sortWidthFirst) rects
orderRectangles 4 rects = sortBy sortHeightFirst rects
orderRectangles 5 rects = sortBy (flip sortHeightFirst) rects
orderRectangles 6 rects = sortBy sortByArea rects
orderRectangles 7 rects = sortBy (flip sortByArea) rects
orderRectangles _ rects = rects 

-- Generates the requested output

generateOutput :: Int -> Result -> Shape -> IO ()
generateOutput 1 r s = printResults r s 
generateOutput 2 r s = writeToFile output 
                       where output = makeTextOutput r s
generateOutput 3 r s = makeElmSource r s
generateOutput _ _ _ = return ()

-- Parses the input points and converts them to rectangle

getRectangles :: [[Double]] -> Shape -> [Rectangle]
getRectangles parsedLines = filterRects rects
                            where poly = fromMaybe [] $ parsePolygons parsedLines
                                  rects = map convertPolyToRectangle poly
                                  

-- Filters out the rectangles which are larger than the stock

filterRects :: [Rectangle] -> Shape -> [Rectangle]
filterRects (r:rs) st@(Stock w h) 
  | (getWidth r <= w && getHeight r <= h) || 
      (getHeight r <= w && getWidth r <= h) = r : filterRects rs st
  | otherwise                           = filterRects rs st
filterRects r _                   = r

-- Converts polygon to rectangle

convertPolyToRectangle :: Shape -> Rectangle
convertPolyToRectangle (Polygon pts) = Rec (0,0) 
                                        (abs (getMaxX pts - getMinX pts)) 
                                        (abs (getMaxY pts - getMinY pts))
convertPolyToRectangle (Stock _ _)   = Rec (0,0) 0 0

-- Generic functions for parsing

parseStock :: [Double] -> Maybe Shape
parseStock [len,wid] = Just (Stock len wid)
parseStock _         = fail "Invalid stock"

parsePolygons :: [[Double]] -> Maybe [Shape]
parsePolygons (dim:count:xs) 
  | length dim `mod` 2 == 0 = case length count of
                                1 -> Just (getPolygons count dim ++ 
                                        fromMaybe [] (parsePolygons xs))
                                _ -> fail "Invalid count"
  | otherwise              = fail "Invalid polygon point"
parsePolygons _            = Nothing

getPolygons :: RealFrac r => [r] -> [Double] -> [Shape]
getPolygons count dim = replicate (getCount count) (Polygon $ getPolygon dim)

getCount :: (Integral b, RealFrac r) => [r] -> b
getCount arr = round $ head arr

getPolygon :: [Double] -> [Point]
getPolygon (x:y:xs) = (x,y) : getPolygon xs
getPolygon _        = []

parseLinesToDouble :: [[String]] -> [[Double]]
parseLinesToDouble = map parseLine

parseLine :: [String] -> [Double]
parseLine  = foldr (\ x -> (++) [read x :: Double]) []

-- String literals

algChoice :: String
algChoice = "Choose Algorithm:\n \
\   (1) MaxRect Best Area Fit \n \
\   (2) MaxRect Best Short Side Fit \n \
\   (3) Guillotine with Axis cut \n \
\   (4) Guillotine with Leftover cut \n \
\   (5) Shelf Best Area Fit\n \
\   (6) Shelf Next Fit \n"
                                
orderChoice :: String
orderChoice = "Want to order pieces by: \n \
\   (1) Input order\n \
\   (2) Sort by width (ascending)\n \
\   (3) Sort by width (descending)\n \
\   (4) Sort by height (ascending)\n \
\   (5) Sort by height (descending)\n \
\   (6) Sort by area (ascending)\n \
\   (7) Sort by area (descending)\n "

outputFormat :: String
outputFormat = "Output to: \n \
\  (1) Console\n \
\  (2) File\n \
\  (3) Elm source\n "