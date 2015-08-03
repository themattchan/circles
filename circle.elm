import Color            exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import List             as       L
import Text

{- Pack circle buttons around a semicircle -}

type alias Point = (Float, Float)
type alias Rad   = Float

-- Given N circles to pack in the semicircle, find the ith angle offset from the
-- positive x-axis, travelling clockwise.
-- centerAngle : Int -> Int -> Float
centerAngle n i = (pi * ((2 * i) - 1)) / (2 * n)

mirrorAngle ang = ang * (-1)

-- Given the radius of the semicircle R, and a list of angle offsets for the
-- centers of the buttons, find the center coordinates of the buttons, relative
-- to the center of the semicircle.
centerCoord : Float -> Rad -> Point
centerCoord r theta = (r * (cos theta), r * (sin theta))

-- Given the center of the semicircle to the Absolute Center (top left corner),
-- translate a coordinate rel to the center to an absolute position.
translate : Point -> Point -> Point
translate (ax,ay) (x,y) = (ax + x, ay + y)

replicate : Int -> List a -> List a
replicate n xs = L.foldl (\_ a -> xs ++ a) [] [1..n]

mkCircle radius colour centre =
  circle radius |> filled colour |> move centre

circles = let centres = L.map
                         (  translate (0, 100)
                         << centerCoord 100
                         << mirrorAngle
                         << centerAngle 5
                         )  [1..5]
              colours = replicate 2 rgbs
          in L.map2 (mkCircle 25) colours centres

button = let (Just c) = L.head rgbs in
         mkCircle 50 c (0,100)

-- Origin is centre of the square. Collage is w, h
main : Element
main = collage 400 200  (button :: circles)

rgbs =  L.map3 rgb
          [64 , 133, 102, 242, 238]
          [174, 158, 102, 161, 87 ]
          [122, 211, 102, 17 , 30 ]
