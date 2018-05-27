{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ParallelListComp #-}

module Main where

import Data.Maybe (fromMaybe)
import Linear

-- |A 2D vector.
type Vector = V2 Rational

-- ##############
-- # Primitives #
-- ##############
-- The denotation of all primtives is a set of points.

-- |A single point in 2D space
--   [[ V2 x y ]] = { (x,y) }
-- Note that for convenience, a point is often used as
--   [[ V2 x y ]] = (x,y)
-- Which denotation is being used should always be clear
-- from the context. 
type Point = Vector

-- |A non-zero length line segment.
--   [[ Seg a b ]]
--      = { a + t (b - a) | t <- R, 0 <= t <= 1 }
--        (a /= b)
data Segment = Seg Point Point

mkSegment :: Point -> Point -> Maybe Segment
mkSegment a b
  | a /= b     = Just (Seg a b)
  | otherwise  = Nothing

-- |A half space split by a line.
--   [[ HalfSpace normal d ]]
--      = { p | p <- R^2, p `dot` normal <= d }
--        (normal /= 0)
data HalfSpace = HalfSpace Vector Rational

mkHalfSpace :: Vector -> Rational -> Maybe HalfSpace
mkHalfSpace normal d
  | normal /= 0  = Just (HalfSpace normal d)
  | otherwise    = Nothing

-- |A convex polygon.
-- The points are the vertices of the polygon and have the following
-- constraints. Let n = length points and p_i = points !! (i mod n):
--      n >= 3
--      points has counter clockwise (CCW) winding
--        <-> crossZ (p_i - p_i-1) (p_i+1 - p_i) > 0
--        --> no duplicate points and no colinear points
--      --> The polygon has non-zero area.
-- 
--   [[ ConvexPolygon points ]]
--      = Intersection of all [[ points ]]
--      (non-zero area)
-- where [[ points ]] is a set of half spaces (see polygonToHalfSpaces).
newtype ConvexPolygon = ConvexPolygon { polygonPoints :: [Point] }
  deriving (Show)

mkConvexPolygon :: [Point] -> Maybe ConvexPolygon
mkConvexPolygon points
  | n >= 3 && hasCCW  = Just (ConvexPolygon points)
  | otherwise         = Nothing
  where
    hasCCW = all [ crossZ (b - a) (c - b) > 0
                    | a <- last points : points
                    | b <- points
                    | c <- tail pointa ++ [head points]
                    ]

-- ####################
-- # Helper Functions #
-- ####################

normSq :: Vector -> Rational
normSq v = v `dot` v

rotate90CW :: Vector -> Vector
rotate90CW (V2 x y) = V2 y (-x)

polygonToEdges :: ConvexPolygon -> [Segment]
polygonToEdges (ConvexPolygon ps) = zipWith Seg ps (tail ps ++ [head ps])

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
    (halfSpace pA (rotate90CW (pB - pA)))


-- ##################
-- # Contains Class #
-- ##################
-- As all primitives denote point sets, [[ contains ]] is naturally defined
-- as set containment:
--     [[ a `contains` b ]] = [[b]] `subsetOrEqual` [[a]]
-- Note this implies that all primitives contain themselves (a `contains` a == true)
class Contains a b where
  contains :: a -> b -> Bool

--   [[ V2 x1 y1 `contains` V2 x2 y2 ]]
--      = { (x1,y1) } `subsetOrEqual` { (x2,y2) }
--      = (x1,y1) == (x2,y2)
instance Contains Point Point where
  contains = (==)

--   [[ HalfSpace normal d `contains` point ]]
--      = point `subsetOrEqual` { p | p <- R^2, p `dot` normal <= d }
--      = point `dot` normal <= d
instance Contains HalfSpace Point where
  contains (HalfSpace normal d) point = point `dot` normal <= d

--   [[ ConvexPolygon points `contains` p ]]
--      = p `subsetOrEqual` Intersection of all [[ points ]]
--      = forall plane <- [[ points ]]. p `subsetOrEqual` plane
--      = forall plane <- [[ points ]]. [[ plane `contains` point ]]
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

-- Using the instance Contains HalfSpace ConvexPolygon:
--   [[ ConvexPolygon pointsA `contains` polygonB ]]
--      = polygonB `subsetOrEqual` Intersection of all [[ pointsA ]]
--      = forall plane <- [[ pointsA ]]. polygonB `subsetOrEqual` plane
--      = forall plane <- [[ pointsA ]]. [[ plane `contains` polygonB ]]
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
--     h1 == h2  <->  exists s s.t. s > 0, s a2 == a1, s b2 == b1, s d2 == d1
-- This check solves for s using s == a1 / a2 or s == b1 / b2, then checks that the remaining
-- conditions above hold. Note that at least one of a2 and b2 is non-zero as half space
-- normals are non-zero.
instance Eq HalfSpace where
  (HalfSpace (V2 a1 b1) d1) == (HalfSpace (V2 a2 b2) d2)
    | a2 /= 0   = let s = a1 / a2 in s > 0 && s * b2 == b1 && s * d2 == d1
    | otherwise = let s = b1 / b2 in s > 0 && s * a2 == a1 && s * d2 == d1

-- |As an instance of Contains ConvexPolygon ConvexPolygon exists, canonicalEq can be used here. 
instance Eq ConvexPolygon where
  (==) = canonicalEq

-- # Approximately Contains

class ApproxContains a b where
  approxContains :: Rational -> a -> b -> Bool

instance ApproxContains ConvexPolygon ConvexPolygon where
  approxContains tolerance a b = distanceSq a b < tolerance * tolerance

-- # Approximately Equal

class ApproxEq a where
  approxEq :: Rational -> a -> a -> Bool
  default approxEq :: (ApproxContains a a) => Rational -> a -> a -> Bool
  approxEq tolerance a b = approxContains tolerance a b && approxContains tolerance b a

instance ApproxEq ConvexPolygon

-- TODO DistanceSq
class DistanceSq a b where
  distanceSq :: a -> b -> Rational
  default distanceSq :: DistanceSq b a => a -> b -> Rational
  distanceSq = flip distanceSq

instance DistanceSq Point Point where
  distanceSq a b = normSq (b - a)

instance DistanceSq Segment Point
instance DistanceSq Point Segment where
  distanceSq p (Seg a b)
    | behindA && behindB = (1 / normSq normal) * ((ap `dot` normal) ^ 2)
    | behindA            = distanceSq p b
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
main = do
  putStrLn "TODO lets do some tests here!"
