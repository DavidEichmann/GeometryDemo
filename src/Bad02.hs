{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Math

import Test.Tasty
import Test.Tasty.HUnit






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
pointWiseApproxEq eps (c:cs) (d:ds) = approxEq eps c d  &&  pointWiseApproxEq cs ds
pointWiseApproxEq _   []     []     = True
pointWiseApproxEq _   []     _      = False
pointWiseApproxEq _   _      []     = False









-- Standard resources

type Vector = (Double, Double)
type Point = (Double, Double)
newtype ConvexPolygon = ConvexPolygon [Point]

class ApproxEq a where
    approxEq :: Double -> a -> a -> Bool


