module Main where

main :: IO ()
main = do
  putStrLn "hello world"

data Point = Point { x :: Double, y :: Double}

point :: (Double, Double) -> Point
point (x,y) = Point x y

pointX :: Point -> Double
pointX (Point x y) = x

pointY :: Point -> Double
pointY (Point x y) = y

instance Eq Point where a == b = abs (pointX a - pointX b) < 0.01 && abs (pointY a - pointY b) < 0.01

data Curve = Curve { list :: [Point] }
curve :: Point -> [Point] -> Curve
curve point list = Curve (point : list)

getList :: Curve -> [Point]
getList (Curve list) = list

connect :: Curve -> Curve -> Curve
connect c1 c2 = Curve (getList c1 ++ getList c2)

