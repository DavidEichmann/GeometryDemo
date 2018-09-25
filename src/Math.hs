{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Math where

import Data.Maybe (fromMaybe)
import Data.List (inits, tails)

import Test.Tasty
import Test.Tasty.HUnit

distance :: (Double, Double) -> (Double, Double) -> Double
distance

dot :: (Double, Double) -> (Double, Double) -> Double
dot (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2)

crossZ :: (Double, Double) -> (Double, Double) -> Double
crossZ (x1, y1) (x2, y2) = (x1 * y2) - (x2 * y1)

normSq :: (Double, Double) -> Double
normSq v = v `dot` v

norm :: (Double, Double) -> Double
norm = sqrt . normSq

rotate90CW :: (Double, Double) -> (Double, Double)
rotate90CW (x, y) = (y, -x)

instance Num (Double, Double) where
  (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  (-) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
  (*) (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger i = (fromInteger i, fromInteger i)
 