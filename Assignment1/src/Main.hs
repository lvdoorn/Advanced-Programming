import Text.Printf

data Point = Point { x :: Double, y :: Double} deriving Show

point :: (Double, Double) -> Point
point (x,y) = Point x y

pointX :: Point -> Double
pointX (Point x y) = x

pointY :: Point -> Double
pointY (Point x y) = y

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
rotatePoint angle p = let a = degToRad angle in point (pointX p * cos a - pointY p * sin a, pointY p * cos a + pointX p * sin a)

rotate :: Curve -> Double -> Curve
rotate c1 angle = Curve (map (rotatePoint angle) $ toList c1)

translatePoint :: (Double, Double) -> Point -> Point
translatePoint (x,y) p = point(x + pointX p, y + pointY p)

getOffset :: Curve -> Point -> (Double, Double)
getOffset c p =  (x2 - x1, y2 - y1) where
  first = head $ toList c
  x1 = pointX first
  x2 = pointX p
  y1 = pointY first
  y2 = pointY p

translate :: Curve -> Point -> Curve
translate c p = Curve (translatePoint (getOffset c p) `map` (toList c))

data Line = Vertical Double | Horizontal Double

reflectPointV :: Double -> Point -> Point
reflectPointV d (Point x y) = Point (x - 2 * (x - d)) y

reflectPointH :: Double -> Point -> Point
reflectPointH d (Point x y) = Point x (y - 2 * (y - d))

reflect :: Curve -> Line -> Curve
reflect c (Vertical d) = Curve ((reflectPointV d) `map` (toList c))
reflect c (Horizontal d) = Curve ((reflectPointH d) `map` (toList c))

bbox :: Curve -> (Point, Point)
bbox (Curve list) = (Point minX minY, Point maxX maxY) where
  xList = pointX `map` list
  yList = pointY `map` list
  minX = minimum xList
  maxX = maximum xList
  minY = minimum yList
  maxY = maximum yList

width :: Curve -> Double
width c = let bb = bbox c 
          in pointX (snd bb) - pointX (fst bb) 

height :: Curve -> Double
height c = let bb = bbox c 
           in pointY (snd bb) - pointY (fst bb) 

normalize :: Curve -> Curve
normalize c = let lowerLeftBboxPoint = fst (bbox c)
                  lowerLeftBboxPointX = pointX lowerLeftBboxPoint
                  lowerLeftBboxPointY = pointY lowerLeftBboxPoint
              in Curve (translatePoint (- lowerLeftBboxPointX, - lowerLeftBboxPointY) `map` (toList c))

createLine :: Point -> Point -> String
createLine (Point x1 y1) (Point x2 y2) = 
  printf ("<line style=\"stroke-width: 2px; stroke: black; fill:white\"" ++
  "x1=\"%.2f\" x2=\"%.2f\" y1=\"%.2f\" y2=\"%.2f\" />") x1 x2 y1 y2

listToLines :: [Point] -> String
listToLines [] = ""
listToLines [_] = ""
listToLines (x:y:xs) = createLine x y ++ "\n" ++ listToLines (y:xs)

toSVG :: Curve -> String
toSVG (Curve list) = "<svg xmlns=\"http://www.w3.org/2000/svg\"width=\"200px\" height=\"200px\" version=\"1.1\"><g>" ++
  listToLines list ++ "</g></svg>"

toFile :: Curve -> FilePath -> IO ()
toFile curve path = writeFile path $ toSVG curve

hilbert :: Curve -> Curve
hilbert c = c0 `connect` c1 `connect` c2 `connect` c3
   where  w = width c
          h = height c
          p = 6

          ch = reflect c $ Vertical 0 -- html svg has 0,0 in top left corner

          c0 = ch `rotate` (-90) `translate` point (w+p+w, h+p+h)
          c1 = c `translate` point (w+p+w, h)
          c2 = c
          c3 = ch `rotate` 90 `translate` point (0, h+p)

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