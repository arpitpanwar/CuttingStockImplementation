import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Random exposing (..)

type alias Width = Float
type alias Height = Float
type alias Point = (Width, Height)
type Rectangle = Rec Point Width Height
type Bin = B (List Rectangle)
type Result = R (List Bin)

main : Element
main = drawResult result

drawResult : Result -> Element
drawResult (R r) = flow down (showWaste :: showAvgWaste :: List.map (
                              \x -> bin bwidth bheight x) r)

showWaste : Element
showWaste = collage 200 25 [toForm (show waste)]

showAvgWaste : Element
showAvgWaste = collage 200 25 [toForm (show avg_waste)]

black : Color
black = rgba 0 0 0 1

rectColor : Rectangle -> Color
rectColor (Rec _ w h) = hashColor (round (w + h))

bin : Width -> Height -> Bin -> Element
bin w h p = collage (round w * 3) (round h * 2) (binBorder w h :: pieces p)

binBorder : Width -> Height -> Form
binBorder w h = outlined (solid black) (rectangle (Rec (0, 0) w h))

pieces : Bin -> List Form
pieces (B p) = List.map (\x -> filled (rectColor x) (rectangle x)) p ++ 
                List.map (\x -> outlined (solid black) (rectangle x)) p
rectangle : Rectangle -> Shape
rectangle (Rec (x, y) w h) = polygon [(x, y),(x + w, y),(x + w, y + h),(x, y + h)]

hashColor : Int -> Color
hashColor i = case i % 9 of
                0 -> rgba 255 0 0 1
                1 -> rgba 0 255 0 1
                2 -> rgba 0 0 255 1
                3 -> rgba 255 255 0 1
                4 -> rgba 255 0 255 1
                5 -> rgba 0 255 255 1
                6 -> rgba 255 0 0 1
                7 -> rgba 123 123 0 1
                8 -> rgba 123 0 123 1
                _ -> rgba 0 0 0 1
bwidth = 50.0
bheight = 100.0
waste = "Total waste: 104250.0"
avg_waste = "Avg waste: 1447.9166666666667"
result = R [B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 20.0 20.0,Rec (30.0,20.0) 20.0 20.0,Rec (30.0,40.0) 20.0 20.0,Rec (30.0,60.0) 20.0 20.0,Rec (30.0,80.0) 20.0 20.0,Rec (0.0,80.0) 20.0 20.0,Rec (20.0,80.0) 10.0 20.0,Rec (20.0,60.0) 10.0 20.0,Rec (0.0,30.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 20.0 20.0,Rec (30.0,20.0) 20.0 20.0,Rec (30.0,40.0) 20.0 20.0,Rec (30.0,60.0) 20.0 20.0,Rec (30.0,80.0) 20.0 20.0,Rec (0.0,80.0) 20.0 20.0,Rec (20.0,80.0) 10.0 20.0,Rec (20.0,60.0) 10.0 20.0,Rec (0.0,30.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 20.0 20.0,Rec (30.0,20.0) 20.0 20.0,Rec (30.0,40.0) 20.0 20.0,Rec (30.0,60.0) 20.0 20.0,Rec (30.0,80.0) 20.0 20.0,Rec (0.0,80.0) 20.0 20.0,Rec (20.0,80.0) 10.0 20.0,Rec (20.0,60.0) 10.0 20.0,Rec (0.0,30.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 20.0 20.0,Rec (30.0,20.0) 20.0 20.0,Rec (30.0,40.0) 20.0 20.0,Rec (30.0,60.0) 20.0 20.0,Rec (30.0,80.0) 20.0 20.0,Rec (0.0,80.0) 20.0 20.0,Rec (20.0,80.0) 10.0 20.0,Rec (20.0,60.0) 10.0 20.0,Rec (0.0,30.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 20.0 20.0,Rec (30.0,20.0) 20.0 20.0,Rec (30.0,40.0) 20.0 20.0,Rec (30.0,60.0) 20.0 20.0,Rec (30.0,80.0) 20.0 20.0,Rec (0.0,80.0) 20.0 20.0,Rec (20.0,80.0) 10.0 20.0,Rec (20.0,60.0) 10.0 20.0,Rec (0.0,30.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 20.0 20.0,Rec (30.0,20.0) 20.0 20.0,Rec (30.0,40.0) 20.0 20.0,Rec (30.0,60.0) 20.0 20.0,Rec (30.0,80.0) 20.0 20.0,Rec (0.0,80.0) 20.0 20.0,Rec (20.0,80.0) 10.0 20.0,Rec (20.0,60.0) 10.0 20.0,Rec (0.0,30.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 20.0 20.0,Rec (30.0,20.0) 20.0 20.0,Rec (30.0,40.0) 20.0 20.0,Rec (30.0,60.0) 20.0 20.0,Rec (30.0,80.0) 20.0 20.0,Rec (0.0,80.0) 20.0 20.0,Rec (20.0,80.0) 10.0 20.0,Rec (20.0,60.0) 10.0 20.0,Rec (0.0,30.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 20.0 20.0,Rec (30.0,20.0) 20.0 20.0,Rec (30.0,40.0) 20.0 20.0,Rec (30.0,60.0) 20.0 20.0,Rec (30.0,80.0) 20.0 20.0,Rec (0.0,80.0) 20.0 20.0,Rec (20.0,80.0) 10.0 20.0,Rec (20.0,60.0) 10.0 20.0,Rec (0.0,30.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 20.0 20.0,Rec (30.0,20.0) 20.0 20.0,Rec (30.0,40.0) 20.0 20.0,Rec (30.0,60.0) 20.0 20.0,Rec (30.0,80.0) 20.0 20.0,Rec (0.0,80.0) 20.0 20.0,Rec (20.0,80.0) 10.0 20.0,Rec (20.0,60.0) 10.0 20.0,Rec (0.0,30.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 20.0 20.0,Rec (30.0,20.0) 20.0 20.0,Rec (30.0,40.0) 20.0 20.0,Rec (30.0,60.0) 20.0 20.0,Rec (30.0,80.0) 20.0 20.0,Rec (0.0,80.0) 20.0 20.0,Rec (20.0,80.0) 10.0 20.0,Rec (20.0,60.0) 10.0 20.0,Rec (0.0,30.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 20.0 20.0,Rec (30.0,20.0) 20.0 20.0,Rec (30.0,40.0) 20.0 20.0,Rec (30.0,60.0) 20.0 20.0,Rec (30.0,80.0) 20.0 20.0,Rec (0.0,80.0) 20.0 20.0,Rec (20.0,80.0) 10.0 20.0,Rec (20.0,60.0) 10.0 20.0,Rec (0.0,30.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 20.0 20.0,Rec (30.0,20.0) 20.0 20.0,Rec (30.0,40.0) 20.0 20.0,Rec (30.0,60.0) 20.0 20.0,Rec (30.0,80.0) 20.0 20.0,Rec (0.0,80.0) 20.0 20.0,Rec (20.0,80.0) 10.0 20.0,Rec (20.0,60.0) 10.0 20.0,Rec (0.0,30.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 20.0 20.0,Rec (30.0,20.0) 20.0 20.0,Rec (30.0,40.0) 20.0 20.0,Rec (30.0,60.0) 20.0 20.0,Rec (30.0,80.0) 20.0 20.0,Rec (0.0,80.0) 20.0 20.0,Rec (20.0,80.0) 10.0 20.0,Rec (20.0,60.0) 10.0 20.0,Rec (0.0,30.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 20.0 20.0,Rec (30.0,20.0) 20.0 20.0,Rec (30.0,40.0) 20.0 20.0,Rec (30.0,60.0) 20.0 20.0,Rec (30.0,80.0) 20.0 20.0,Rec (0.0,80.0) 20.0 20.0,Rec (20.0,80.0) 10.0 20.0,Rec (20.0,60.0) 10.0 20.0,Rec (0.0,30.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 20.0 20.0,Rec (30.0,20.0) 20.0 20.0,Rec (30.0,40.0) 20.0 20.0,Rec (30.0,60.0) 20.0 20.0,Rec (30.0,80.0) 20.0 20.0,Rec (0.0,80.0) 20.0 20.0,Rec (20.0,80.0) 10.0 20.0,Rec (20.0,60.0) 10.0 20.0,Rec (0.0,30.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 20.0 20.0,Rec (30.0,20.0) 20.0 20.0,Rec (30.0,40.0) 20.0 20.0,Rec (30.0,60.0) 20.0 20.0,Rec (30.0,80.0) 20.0 20.0,Rec (0.0,80.0) 20.0 20.0,Rec (0.0,60.0) 20.0 20.0,Rec (0.0,30.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 20.0 20.0,Rec (30.0,20.0) 20.0 20.0,Rec (30.0,40.0) 20.0 20.0,Rec (30.0,60.0) 5.0 35.0,Rec (0.0,95.0) 35.0 5.0,Rec (0.0,60.0) 30.0 30.0,Rec (0.0,30.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0],B [Rec (0.0,0.0) 30.0 30.0,Rec (30.0,0.0) 5.0 35.0,Rec (35.0,30.0) 5.0 35.0,Rec (30.0,35.0) 5.0 35.0,Rec (0.0,70.0) 30.0 30.0,Rec (0.0,35.0) 30.0 30.0,Rec (35.0,65.0) 5.0 35.0,Rec (40.0,65.0) 5.0 35.0,Rec (45.0,65.0) 5.0 35.0]]