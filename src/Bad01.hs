{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Math

import Test.Tasty
import Test.Tasty.HUnit






-- Invariants:
-- * Implicitely closed
-- * CCW winding: describes all vertices in CCW order.
newtype ConvexPolygon = ConvexPolygon [Point]

-- Hinge questions
main :: IO ()
main = defaultMain $ testGroup "Bad01" [
    testGroup "Hinge questions" [
        testCase "Exactly Equal Triangles" $
            approxEq 0.0001
                ConvexPolygon [(0,0), (1,0), (0,1)]
                ConvexPolygon [(0,0), (1,0), (0,1)]
            @?= expected True,

        testCase "Square /= Triangle" $
            approxEq 0.0001
                ConvexPolygon [(0,0), (1,0), (1,1), (0,1)]
                ConvexPolygon [(0,0), (1,0), (0,1)]
            @?= expected True,

        testCase "Slightly different triangles." $
            approxEq 0.0001
                ConvexPolygon [(0  ,0), (1,0), (0,1)]
                ConvexPolygon [(0.1,0), (1,0), (0,1)]
            @?= expected True,

        testCase "Slightly different triangles, but large enough epsilon value." $
            approxEq 0.2
                ConvexPolygon [(0  ,0), (1,0), (0,1)]
                ConvexPolygon [(0.1,0), (1,0), (0,1)]
            @?= expected True
        ]
    missedCasesGroup
    ]

expected = id
unexpected = id

{-
First attempt
The programmer thinks about exact equality, then adapts it to approximate equality.
-}

-- ConvexPolygon exact equality is point-wise exact equality
instance Eq ConvexPolygon where
    (ConvexPolygon as) == (ConvexPolygon bs) = as == bs

-- ConvexPolygon approximate equality is point-wise approximate equality
instance ApproxEq ConvexPolygon where
    approxEq eps (ConvexPolygon as) (ConvexPolygon bs)    =  pointWiseApproxEq eps as bs
        
instance ApproxEq Point where
    approxEq eps a                  b                     =  distance a b <= eps

pointWiseApproxEq :: Double -> [Point] -> [Point] -> Bool
pointWiseApproxEq eps (a:as) (b:bs) = approxEq eps a b  &&  pointWiseApproxEq as bs
pointWiseApproxEq eps []     []     = True
pointWiseApproxEq eps []     bs     = False
pointWiseApproxEq eps as     []     = False




{-
But we miss some cases:
-}
missedCasesGroup :: TestGroup
missedCasesGroup = let
    a = (0,0)
    b = (1,0)
    c = (0,1)
  in testGroup "Missed Cases" [

    testCase "Duplicate Points" $
        approxEq 0.0001
            ConvexPolygon [a, b, c   ]
            ConvexPolygon [a, b, c, c]
        @?= unexpected False,

    testCase "Colinear Points" $
        approxEq 0.0001
            ConvexPolygon [a,  (a+b)/2,  b, c]
            ConvexPolygon [a,            b, c]
        @?= unexpected False,

    testCase "Rotated order" $
        approxEq 0.0001
            ConvexPolygon [a, b, c   ]
            ConvexPolygon [   b, c, a]
        @?= unexpected False,

    testCase "Different Winding (reverse order)" $
        approxEq 0.0001
            ConvexPolygon [a, b, c]
            ConvexPolygon [c, b, a]
        @?= unexpected False,

    ]





-- Standard resources

type Vector = (Double, Double)
type Point = (Double, Double)

class ApproxEq a where
    approxEq :: Double -> a -> a -> Bool


