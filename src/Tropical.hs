module Tropical where

import Prelude as Pre

import MMP ( MinMaxPlus(..) )

data MMP a = Tropical a | Top deriving (Show, Eq)

instance (Ord a, Num a) => MinMaxPlus (MMP a) where
  zero = Top
  one = Tropical 0
  max x Top = x
  max Top y = y
  max (Tropical x) (Tropical y) = Tropical (x `Pre.max` y)
  min x Top = x
  min Top y = y
  min (Tropical x) (Tropical y) = Tropical (x `Pre.min` y)
  plus _ Top = zero
  plus Top _ = zero
  plus (Tropical x) (Tropical y) = Tropical (x + y)
  slash _ Top = zero
  slash Top _ = zero
  slash (Tropical x) (Tropical y) = Tropical (-x + y)