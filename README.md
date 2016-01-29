#Project for CIS 552 Advanced Programming

By Stuart Wagner (wagners), Arpit Panwar (arpanwar), Pedro Samora (psamora)

Implementation of Bin Packing problem in Haskell.

Format of input data file
- The first line will contain the dimensions of the stock
- Subsequent lines will contain the Dimensions of each edge in format x1,y1,x2,y2 ... followed by how many times the shape is to be repeated
- Sample input data file included

Our program works as follows. We have Main.hs which parses input from a txt file, takes in input as an argument.
The input file is parsed and user is given an option of ordering the data and later the option of choosing the algorithm that should be run on the input data.
The program than gives the user an option to choose the output method , the user can choose between printing the output to console, writing to a file or generating an elm file which will display the result.

Structure of the Project:
Main.hs              : The main file which interacts with the user and parses the input and runs the algorithms
BinPack.hs           : The BinPack module file which contains common data structures and functions which are shared among all the algorithms 
BinPack/MaxRect.hs   : Implementation of Best Area Fit and Best Short Side Fit variants of MaxRect algorithm
BinPack/Guillotine.hs: Implementation of Axis Cut and LeftOver cut variants of Guillotine algorithms
BinPack/Shelf.hs     : Implementation of Best Area Fit and Next Fit variants of Shelf Algorithms
extra/template.elm         : Template used to generate elm output
extra/test.txt             : Sample input file

Libraries: System.Environment, System.IO, Data.Maybe, Text.Read, Data.List, Test.HUnit, Control.Monad, Control.Applicative

Compiling and Running:

1. Using Ghc : i. ghc --make Main.hs
               ii. Main.exe extra/test.txt

2. Using runGhc : runGhc Main.hs extra/test.txt

