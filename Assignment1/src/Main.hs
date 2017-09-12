module Curves where

import Text.Printf

data Point = Point { x :: Double, y :: Double} deriving Show

point :: (Double, Double) -> Point
point (x,y) = Point x y

pointX :: Point -> Double
pointX (Point x _) = x

pointY :: Point -> Double
pointY (Point _ y) = y

instance Eq Point where a == b = abs (pointX a - pointX b) < 0.01 && abs (pointY a - pointY b) < 0.01

data Curve = Curve { list :: [Point] } deriving Show
curve :: Point -> [Point] -> Curve
curve point list = Curve (point : list)

toList :: Curve -> [Point]
toList (Curve list) = list

connect :: Curve -> Curve -> Curve
connect c1 c2 = Curve (toList c1 ++ toList c2)

-- Converts a number of degrees to radians
degToRad :: Double -> Double
degToRad deg = deg * pi / 180

-- Rotates a point around the origin by angle degrees
rotatePoint :: Double -> Point -> Point
rotatePoint angle (Point x y) = let a = degToRad angle in point (x * cos a - y * sin a, y * cos a + x * sin a)

-- Rotates a point around the origin
rotate :: Curve -> Double -> Curve
rotate c1 angle = Curve (map (rotatePoint angle) $ toList c1)

-- Translate a point by adding a given offset
translatePoint :: (Double, Double) -> Point -> Point
translatePoint (x,y) (Point px py) = point(x + px, y + py)

-- Calculates offset between a point and the first point of a curve
getOffset :: Curve -> Point -> (Double, Double)
getOffset c (Point x2 y2) =  (x2 - x1, y2 - y1) where
  Point x1 y1 = head $ toList c

-- Translates a curve s.t. the starting point will be ~p
translate :: Curve -> Point -> Curve
translate c p = Curve (translatePoint (getOffset c p) `map` (toList c))

data Line = Vertical Double | Horizontal Double

-- Reflects a point in the vertical line given by d
reflectPointV :: Double -> Point -> Point
reflectPointV d (Point x y) = Point (x - 2 * (x - d)) y

-- Reflects a point in the horizontal line given by d
reflectPointH :: Double -> Point -> Point
reflectPointH d (Point x y) = Point x (y - 2 * (y - d))

-- Reflects a curve in given vertical or horizontal line
reflect :: Curve -> Line -> Curve
reflect c (Vertical d) = Curve ((reflectPointV d) `map` (toList c))
reflect c (Horizontal d) = Curve ((reflectPointH d) `map` (toList c))

-- Calculates the lower left and upper right points of the curve's bounding box
bbox :: Curve -> (Point, Point)
bbox (Curve list) = (Point minX minY, Point maxX maxY) where
  xList = pointX `map` list
  yList = pointY `map` list
  minX = minimum xList
  maxX = maximum xList
  minY = minimum yList
  maxY = maximum yList

-- Calculates the width of a curve
width :: Curve -> Double
width c = let bb = bbox c 
          in pointX (snd bb) - pointX (fst bb) 

-- Calculates the height of a curve+
height :: Curve -> Double
height c = let bb = bbox c 
           in pointY (snd bb) - pointY (fst bb) 

-- Normalizes a curve by translating it into the first quadrant
normalize :: Curve -> Curve
normalize c = let lowerLeftBboxPoint = fst (bbox c)
                  lowerLeftBboxPointX = pointX lowerLeftBboxPoint
                  lowerLeftBboxPointY = pointY lowerLeftBboxPoint
              in Curve (translatePoint (- lowerLeftBboxPointX, - lowerLeftBboxPointY) `map` (toList c))

-- Generate a line of the svg representation
createLine :: Point -> Point -> String
createLine (Point x1 y1) (Point x2 y2) = 
  printf ("<line style=\"stroke-width: 2px; stroke: black; fill:white\"" ++
  "x1=\"%.2f\" x2=\"%.2f\" y1=\"%.2f\" y2=\"%.2f\" />") x1 x2 y1 y2

-- Generates svg representation of a curve given by a list of points
listToLines :: [Point] -> String
listToLines [] = ""
listToLines [_] = ""
listToLines (x:y:xs) = createLine x y ++ "\n" ++ listToLines (y:xs)

-- Generates the svg representation of a curve
toSVG :: Curve -> String
toSVG (Curve list) = "<svg xmlns=\"http://www.w3.org/2000/svg\"width=\"200px\" height=\"200px\" version=\"1.1\"><g>" ++
  listToLines list ++ "</g></svg>"

-- Writes a curve to file
toFile :: Curve -> FilePath -> IO ()
toFile curve path = writeFile path $ toSVG curve

-- Reverses a curve
rev :: Curve -> Curve
rev (Curve list) = (Curve $ reverse list)

-- Creates a hilbert curve
hilbert :: Curve -> Curve
hilbert c = c0 `connect` c1 `connect` c2 `connect` c3
   where  w = width c
          h = height c
          p = 6

          ch = reflect c $ Vertical 0

          c0 = ch `rotate` (-90) `translate` point (w+p+w, h+p+h)
          c1 = c `translate` point (w+p+w, h)
          c2 = c
          c3 = ch `rotate` 90 `translate` point (0, h+p)

-- Creates a peano curve
peano :: Curve -> Curve
peano c = c0 `connect` c1 `connect` c2 `connect` c3 `connect` c4 `connect` c5 `connect` c6 `connect` c7 `connect` c8
  where w = width c
        h = height c
        p = 6
        ch = reflect c $ Vertical 0
        c0 = c `translate` point (w+p+w+p+w, 0)
        c1 = ch `translate` point (w+p+w+p, h+p)
        c2 = c `translate` point (w+p+w+p+w, h+p+h+p)
        c3 = ch `rotate` 180 `translate` point (w+p+w, h+p+h+p+h)
        c4 = c `rotate` 180 `translate` point (w+p, h+p+h)
        c5 = ch `rotate` 180 `translate` point (w+p+w, h)
        c6 = c
        c7 = ch `translate` point (0, p+h)
        c8 = c `translate` point (w, h+p+h+p)

-- Draws dragon curve iteratively by rotating input curve around its endpoint and adding the result to the curve
dragon :: Curve -> Curve
dragon c = normalize $ c `connect` c1 -- run as dragon Curve([Point 5 0, Point 0 0])
  where a = 90
        c1 = (rev c `rotate` a) `translate` (last $ toList c)

-- Calls dragon recursively
-- Try: $> dragonr (curve point (5,0)) [point (0,0)] 10
dragonr :: Curve -> Int -> Curve
dragonr c 0 = c
dragonr c i = dragonr (dragon c) (i - 1)