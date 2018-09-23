{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Main (
  Vector,
  Point,
  Segment,
  mkSeg,
  unsafeSeg,
  HalfSpace,
  mkHalfSpace,
  unsafeHalfSpace,
  ConvexPolygon,
  mkConvexPolygon,
  unsafeConvexPolygon,
  Contains(..),
  DistanceSq(..),
  ApproxEq(..),
  main
) where

import Data.Maybe (fromMaybe)
import Data.List (inits, tails)
-- import Linear

import Test.Tasty
import Test.Tasty.HUnit

-- ##############
-- # Primitives #
-- ##############
-- The denotation of all primtives is a set of points.

-- |A 2D vector.
type Vector = (Double, Double)

-- |A single point in 2D space
--   [[ V2 x y ]] = { (x,y) }
-- Note that for convenience, a point is often used as
--   [[ V2 x y ]] = (x,y)
-- Which denotation is being used should always be clear
-- from the context. 
type Point = (Double, Double)

-- |A (non-zero length) line segment.
--   [[ Seg a b ]]
--      = { a + t (b - a) | t <- R, 0 <= t <= 1 }
--        (a /= b)
data Segment = Seg Point Point
  deriving (Show)

mkSeg :: Point -> Point -> Maybe Segment
mkSeg a b
  | a /= b     = Just (Seg a b)
  | otherwise  = Nothing

unsafeSeg :: Point -> Point -> Segment
unsafeSeg a b = fromMaybe (error "Invalid Segment.") (mkSeg a b)

-- |A half space. This is a binary division of 2D space along a line.
-- All points on the line and on one side of the line are included in
-- the half space. The normal is a non-zero vector perpendicular to the
-- line and points tward the side of the line that is NOT include in
-- half space. The d parameter defines the translation of the line along
-- the normal.
--
--   [[ HalfSpace normal d ]]
--      = { p | p <- R^2, p `dot` normal <= d }
--        (normal /= 0)
data HalfSpace = HalfSpace Vector Double
  deriving (Show)

mkHalfSpace :: Vector -> Double -> Maybe HalfSpace
mkHalfSpace normal d
  | normal /= 0  = Just (HalfSpace normal d)
  | otherwise    = Nothing

unsafeHalfSpace :: Vector -> Double -> HalfSpace
unsafeHalfSpace normal d = fromMaybe (error "Invalid HalfSpace.") (mkHalfSpace normal d)

-- |A convex polygon.
-- Represented as a list of vertices that have the following constraints. 
-- Let n = length points and p_i = points !! (i mod n):
--      n >= 3
--      forall i,j, 0 <= i < n, j /= i, j /= i+1.
--                  (p_i - p_j) `cross` (p_i+1 - p_i) > 0
--
-- This means there are at least 3 vertices and they are listed in counter
-- clockwise (CCW) order. This imples that the polygon is convex, has
-- non-zero area, and the list of points contains no duplicates.
-- 
--   [[ ConvexPolygon points ]]
--      = Intersection of all the polygon's half spaces
--      (non-zero area)
--
-- where the polygon's half spaces = [[ polygonToHalfSpaces (ConvexPolygon points) ]]
newtype ConvexPolygon = ConvexPolygon { polygonPoints :: [Point] }
  deriving (Show)

mkConvexPolygon :: [Point] -> Maybe ConvexPolygon
mkConvexPolygon points
  | n >= 3 && hasCCW  = Just (ConvexPolygon points)
  | otherwise         = Nothing
  where
    n = length points
    hasCCW = and [ crossZ (b - a) (c - b) > 0
                    | a:b:cs <- init $ zipWith (++) (tails points) (inits points)
                    , c <- cs
                    ]

unsafeConvexPolygon :: [Point] -> ConvexPolygon
unsafeConvexPolygon points = fromMaybe (error "Invalid ConvexPolygon.") (mkConvexPolygon points)

-- ####################
-- # Helper Functions #
-- ####################

dot :: (Double, Double) -> (Double, Double) -> Double
dot (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2)

crossZ :: (Double, Double) -> (Double, Double) -> Double
crossZ (x1, y1) (x2, y2) = (x1 * y2) - (x2 * y1)

normSq :: Vector -> Double
normSq v = v `dot` v

rotate90CW :: Vector -> Vector
rotate90CW (x, y) = (y, -x)

polygonToEdges :: ConvexPolygon -> [Segment]
polygonToEdges (ConvexPolygon ps) = zipWith Seg ps (tail ps ++ [head ps])

instance Num (Double, Double) where
  (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  (-) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
  (*) (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger i = (fromInteger i, fromInteger i)
  

-- |Each edge (p_i, p_i+1) of a polygon represents a half space.
-- Both p_i and p_i+1 lie on the line defining that half space.
-- There are no duplicate points so p_i /= p_i+1.
-- Due to counter clockwise winding, the normal must face to the right of the
-- edge (relative to walking from p_i to p_i+1). This means the normal faces out
-- of the polygon.
polygonToHalfSpaces :: ConvexPolygon -> [HalfSpace]
polygonToHalfSpaces = fmap edgeToHalfSpace . polygonToEdges  where
  edgeToHalfSpace :: Segment -> HalfSpace
  edgeToHalfSpace (Seg pA pB) = fromMaybe
    (error "Impossibility: ConvexPolygon's points contain no duplicate points")
    (mkHalfSpace normal d)
    where
      normal = rotate90CW (pB - pA)
      d = normal `dot` pA

-- ##################
-- # Contains Class #
-- ##################
-- As all primitives denote point sets, [[ contains ]] is naturally defined
-- as set containment:
--     [[ a `contains` b ]] = [[b]] `subsetOrEqual` [[a]]
-- Note this implies that all primitives contain themselves (a `contains` a == true)
class Contains a b where
  contains :: a -> b -> Bool

--   [[ (x1, y1) `contains` (x2, y2) ]]
--      = { (x1,y1) } `subsetOrEqual` { (x2,y2) }
--      = (x1,y1) == (x2,y2)
instance Contains Point Point where
  contains = (==)

--   [[ HalfSpace normal d `contains` point ]]
--      = point `subsetOrEqual` { p | p <- R^2, p `dot` normal <= d }
--      = point `dot` normal <= d
instance Contains HalfSpace Point where
  contains (HalfSpace normal d) point = point `dot` normal <= d

--   [[ polygon `contains` p ]]
--      = p `subsetOrEqual` (Intersection of all the polygon's half spaces)
--      = for all halfSpace in (the polygon's half spaces).
--            p `subsetOrEqual` halfSpace
--      = for all halfSpace in [[ polygonToHalfSpaces (polygon) ]].
--            [[ halfSpace `contains` point ]]
instance Contains ConvexPolygon Point where
  contains polygon point = all (`contains` point) (polygonToHalfSpaces polygon)

--   [[ HalfSpace normal d `contains` Seg a b ]]
--      = forall p <- { a + t (b - a) | t <- R, 0 <= t <= 1 }.
--                 p `elementOf` { p | p <- R^2, p `dot` normal <= d }
--      = forall t <- R, 0 <= t <= 1.
--                 (a + t (b - a)) `dot` normal <= d
--      = forall t <- R, 0 <= t <= 1.
--                 t ((b - a) `dot` normal) <= d - (a  `dot` normal)
--      = forall t <- R, 0 <= t <= 1.
--                 t            u           <=         v
--   As u and v are constant, it is clear that we must only check the
--   inequality holds for t = 0 and t = 1.
--      = 0 <= v && u <= v
--   This is equivalent to checking that the half space contains point a and b
--      =  HalfSpace normal d `contains` a  &&  HalfSpace normal d `contains` b
instance Contains HalfSpace Segment where
  contains halfSpace (Seg a b) = halfSpace `contains` a && halfSpace `contains` b

-- As convex polygons are convex, all points in the polygon can be intersected by
-- creating a segment from 2 points on the perimeter of the polyon. This is a 2 way implication
-- so gives an equivalent denotation of:
--   [[ ConvexPolygon points ]]
--      = Union of [[ Seg a b ]] forall a,b <- [[ polygonToEdges (ConvexPolygon points) ]]
-- It is established at the "Contains HalfSpace Segment" instance that
--      halfSpace `contains` a && halfSpace `contains` b  <->  halfSpace `contains` (Seg a b)
-- hence here we can simply check that the half space contains all polygon vertices:
--   [[ HalfSpace normal d `contains` ConvexPolygon points ]]
--      = halfSpace `contains` edge  forall edge <- [[ polygonToEdges (ConvexPolygon points) ]] 
--      = halfSpace `contains` point forall point <- points
instance Contains HalfSpace ConvexPolygon where
  contains halfSpace (ConvexPolygon points) = all (halfSpace `contains`) points

-- Using the instance of Contains HalfSpace ConvexPolygon:
--   [[ ConvexPolygon pointsA `contains` polygonB ]]
--      = polygonB `subsetOrEqual` Intersection of all [[ pointsA ]]
--      = forall halfSpace <- [[ pointsA ]]. polygonB `subsetOrEqual` halfSpace
--      = forall halfSpace <- [[ pointsA ]]. [[ halfSpace `contains` polygonB ]]
instance Contains ConvexPolygon ConvexPolygon where
  contains polygonA polygonB = all (`contains` polygonB) (polygonToHalfSpaces polygonA)

-- |As primitives denote point sets, equality is simply set equality. Hence:
--     a == b  <->  a `subsetOrEqual` b && b `subsetOrEqual` a
-- In this case subsetOrEqual is just the contains method.
canonicalEq :: (Contains a a) => a -> a -> Bool
canonicalEq a b = a `contains` b && b `contains` a

-- |Equality between Half spaces h1 and h2 requires that their inequalities are equivalent:
--     a1 x + b1 y <= d1  <->  a2 x + b2 y <= d2
-- This is true iff (a1,b1,d1) is some positive scale, s > 0, of (a2,b2,d2). Hence:
--     h1 == h2  <->  exists s. s > 0, s a2 == a1, s b2 == b1, s d2 == d1
-- This check solves for s using s == a1 / a2 or s == b1 / b2, then checks that the remaining
-- conditions above hold. Note that at least one of a2 and b2 is non-zero as half space
-- normals are non-zero.
instance Eq HalfSpace where
  (HalfSpace (a1, b1) d1) == (HalfSpace (a2, b2) d2)
    | a2 /= 0   = let s = a1 / a2 in s > 0 && s * b2 == b1 && s * d2 == d1
    | otherwise = let s = b1 / b2 in s > 0 && s * a2 == a1 && s * d2 == d1

-- |As an instance of Contains ConvexPolygon ConvexPolygon exists, canonicalEq can be used here. 
instance Eq ConvexPolygon where
  (==) = canonicalEq

-- ##########################
-- # Approximately Contains #
-- ##########################
-- Primitive a is said to approximately contain primitive b if all points in b are
-- within tolerance (minimum) distance of a.
--   [[ approxContains tolerance a b ]]
--      = forall pointB <- b. in distance pointB a <= tolerance
--      = forall pointB <- b. (min from pointA <- a of distance pointA pointB) <= tolerance
class ApproxContains a b where
  approxContains :: Double -> a -> b -> Bool

instance ApproxContains ConvexPolygon ConvexPolygon where
  -- |Consider the area where distance to polygon a <= tolerance. This area is an
  -- expanded polygonA with rounded perimeters at the vertices. Most importantly, the area
  -- is convex, so we can apply similar logic to the instance of
  -- Contains HalfSpace ConvexPolygon. Specifically, it is sufficient to check that all
  -- vertices of polygonB are within tolerance distance to a.
  approxContains tolerance polygonA (ConvexPolygon pointsB) = all (\pointB -> distanceSq pointB polygonA <= toleranceSq) pointsB
    where
      toleranceSq = tolerance ^ 2

-- #######################
-- # Approximately Equal #
-- #######################
--   [[ approxEq tolerance a b]]
--      = approxContains tolerance a b && approxContains tolerance b a
class ApproxEq a where
  approxEq :: Double -> a -> a -> Bool
  default approxEq :: (ApproxContains a a) => Double -> a -> a -> Bool
  approxEq tolerance a b = approxContains tolerance a b && approxContains tolerance b a

instance ApproxEq ConvexPolygon

-- ####################
-- # Distance Squared #
-- ####################
-- This is always the minimum distance.
--   [[ distanceSq a b ]] = (min pA <- a, pB <- b. distance pA pB) ^ 2
class DistanceSq a b where
  distanceSq :: a -> b -> Double
  default distanceSq :: DistanceSq b a => a -> b -> Double
  distanceSq = flip distanceSq

-- |Distance squared between points is just euclidian distance squared.
instance DistanceSq Point Point where
  distanceSq a b = normSq (b - a)

instance DistanceSq Segment Point
instance DistanceSq Point Segment where
  distanceSq p (Seg a b)
    -- Either the point on the segment closest to p is inbetween the end points...
    | behindA && behindB = (1 / normSq normal) * ((ap `dot` normal) ^ 2)
    -- Or it is end point b...
    | behindA            = distanceSq p b
    -- Else it is end point a...
    | otherwise          = distanceSq p a
    where
      ap = p - a
      segDir = b - a
      normal = rotate90CW segDir
      behindA = (p - a) `dot` segDir > 0
      behindB = (p - b) `dot` segDir < 0

instance DistanceSq Segment Segment where
  distanceSq a@(Seg a1 a2) b@(Seg b1 b2)
    -- Either the segments intersect and hence the distance is 0...
    | intersecting = 0
    -- Or the closest point is on one of the segment end points.
    | otherwise    = minimum
      [ distanceSq a b1
      , distanceSq a b2
      , distanceSq b a1
      , distanceSq b a2
      ]
    where
      intersecting = a `straddles` b && b `straddles` a

      straddles :: Segment -> Segment -> Bool
      straddles (Seg c1 c2) (Seg d1 d2) = sideD1 * sideD2 < 0
        where
          dir = c2 - c1
          sideD1 = crossZ (d1 - c1) dir
          sideD2 = crossZ (d2 - c1) dir

instance DistanceSq ConvexPolygon Point
instance DistanceSq Point ConvexPolygon where
  distanceSq point polygon
    -- There are 2 possibilities.
    -- Either the point is inside the polygon and hence distance is 0...
    | polygon `contains` point = 0
    -- Or the closest point on the polygon is on one of its edges.
    | otherwise                = minimum $ distanceSq point <$> polygonToEdges polygon

instance DistanceSq ConvexPolygon ConvexPolygon where
  distanceSq polygonA polygonB
    -- Either the polygons intersect and the distance is 0...
    | intersecting = 0
    -- Or the closest points are on the edges of the polygons.
    | otherwise    = minimum [distanceSq segA segB | segA <- polygonToEdges polygonA
                                                   , segB <- polygonToEdges polygonB]
    where
      intersecting = any (polygonA `contains`) (polygonPoints polygonB)
                  || any (polygonB `contains`) (polygonPoints polygonA)



main :: IO ()
main = defaultMain $ testGroup "Unit Tests"
  [ testGroup "Eq ConvexPolygon"
    [ testGroup "Equal"
      [ testCase "Rotated point list 01" (let
          polygonA = unsafeConvexPolygon [(0, 0), (2, 0), (1, 1)] 
          polygonB = unsafeConvexPolygon [(2, 0), (1, 1), (0, 0)] 
        in polygonA == polygonB @?= True) 
      , testCase "Rotated point list 02" (let
          polygonA = unsafeConvexPolygon [(0, 0), (2, 0), (2, 2), (1, 3), (0, 2)] 
          polygonB = unsafeConvexPolygon [(1, 3), (0, 2), (0, 0), (2, 0), (2, 2)] 
        in polygonA == polygonB @?= True) 
      ]
    , testGroup "Not Equal"
      [ testCase "Slight difference 01" (let
          polygonA = unsafeConvexPolygon [(0, 0), (2, 0), (1, 1)] 
          polygonB = unsafeConvexPolygon [(2, 0), (1, 1), (0, 1e-10)] 
        in polygonA == polygonB @?= False) 
      , testCase "Missing vertex" (let
          polygonA = unsafeConvexPolygon [(0, 0), (2, 0), (2, 2), (1, 3), (0, 2)] 
          polygonB = unsafeConvexPolygon [(1, 3), (0, 2), (0, 0), (2, 2)] 
        in polygonA == polygonB @?= False) 
      , testCase "Containing polygon" (let
          polygonA = unsafeConvexPolygon [(0, 0), (2, 0), (2, 2), (1, 3), (0, 2)] 
          polygonB = unsafeConvexPolygon [(1, 3), (0, 2), (0, 0), (2, 2)] 
        in polygonA == polygonB @?= False) 
      ]
    ]
  , testGroup "Approximately Equal ConvexPolygon"
    [ testCase "Similar polygon with sufficient tolerance." (let
          polygonA = unsafeConvexPolygon [(0, 0)  , (2,   2), (0, 3)  , (-2,   2)] 
          polygonB = unsafeConvexPolygon [(0, 0.1), (2.1, 2), (0, 3.1), (-1.9, 2)]
          tolerance = 0.1001
      in approxEq tolerance polygonA polygonB @?= True)
    , testCase "Similar polygon with insufficient tolerance." (let
          polygonA = unsafeConvexPolygon [(0, 0)  , (2,   2), (0, 3)  , (-2,   2)] 
          polygonB = unsafeConvexPolygon [(0, 0.1), (2.1, 2), (0, 3.1), (-1.9, 2)]
          tolerance = 0.09
      in approxEq tolerance polygonA polygonB @?= False)
    , testCase "Approximately equal polygons with different numbers of vertices." (let
          polygonA = unsafeConvexPolygon [(0, 0), (2, 2), (0, 3), (-2, 2)] 
          polygonB = unsafeConvexPolygon [(0, 0), (1.1, 1), (2, 2), (0.1, 3), (0, 3), (-2, 2), (-2, 1.9)]
          tolerance = 0.1
      in approxEq tolerance polygonA polygonB @?= True)
    , testCase "Approximately equal polygons with different numbers of vertices and no approximately equal vertices." (let
          polygonA = unsafeConvexPolygon [(0, 0), (5, 0), (5, 5), (0, 5)] 
          polygonB = unsafeConvexPolygon [(-0.1, 0.1), (0.1, -0.1), (4.9, -0.1), (5.1, 0.1), (5.1, 4.9), (4.9, 5.1), (0.1, 5.1), (-0.1, 4.9)]
          tolerance = 0.1
      in approxEq tolerance polygonA polygonB @?= True)
    , testCase "Not Approximately equal polygons with different numbers of vertices and no approximately equal vertices." (let
          polygonA = unsafeConvexPolygon [(0, 0), (5, 0), (5, 5), (0, 5)] 
          polygonB = unsafeConvexPolygon [(-0.1, 0.1), (0.1, -0.1), (4.9, -0.1), (5.1, 0.1), (5.1, 4.9), (4.9, 5.1), (0.1, 5.1), (-0.1, 4.9)]
          tolerance = 0.09
      in approxEq tolerance polygonA polygonB @?= False)
    ]
  ]

