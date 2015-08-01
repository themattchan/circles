import Html

{- Pack circle buttons on a semicircle -}
type alias Point = (Double, Double)
type alias Rad   = Double

-- Given N circles to pack in the semicircle, find the ith angle offset from the
-- positive x-axis, travelling clockwise.
centerAngle : Int -> Int -> Rad
centerAngle n i = (pi * (2*i -1)) / (2*n)

-- Given the radius of the semicircle R, and a list of angle offsets for the
-- centers of the buttons, find the center coordinates of the buttons, relative
-- to the center of the semicircle.
centerCoord : Double -> Rad -> Point
centerCoord r theta = (r * (cos theta), r * (sin theta))

-- Given the center of the semicircle to the Absolute Center (top left corner),
-- translate a coordinate rel to the center to an absolute position.
translateToAbs : Point -> Point -> Point
translateToAbs (ax,ay) (x,y) = (ax + x, ay + y)
