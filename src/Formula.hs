module Formula where

import MMP
import Tropical (MMP)

-- Eq Instanceを書くべき うまくいかないケースが出てくるはず
data Formula a 
  = Max (Formula a) (Formula a) 
  | Min (Formula a) (Formula a) 
  | Plus (Formula a) (Formula a)
  | Top
  | One
  | Numerical (MMP Int)
  | Variable a
  deriving (Show, Eq)

instance Eq a => MinMaxPlus (Formula a) where
  zero = Top
  one = One
  max x Top = x
  max Top y = y
  max x y | x == y = x
  max x y = Max x y
  min x Top = x
  min Top y = y
  min x y | x == y = x
  min x y = Min x y
  plus _ Top = Top
  plus Top _ = Top
  plus x One = x
  plus One y = y
  plus x y | x == y = x
  plus x y = Plus x y
  slash = undefined

compute :: (MinMaxPlus a) => Formula a -> MMP Int
compute Top = zero
compute One = one
compute (Max (Numerical a) (Numerical b)) = a @+ b
compute (Max (Numerical a) b) = a @+ compute b
compute (Max a (Numerical b)) = compute a @+ b
compute (Min (Numerical a) (Numerical b)) = a @+ b
compute (Min (Numerical a) b) = a @+ compute b
compute (Min a (Numerical b)) = compute a @+ b
compute (Plus (Numerical a) (Numerical b)) = a @+ b
compute (Plus (Numerical a) b) = a @+ compute b
compute (Plus a (Numerical b)) = compute a @+ b

replace :: Formula a -> (a -> Formula a) -> Formula a
replace (Max a b) f = Max (replace a f) (replace b f)
replace (Min a b) f = Min (replace a f) (replace b f)
replace (Plus a b) f = Plus (replace a f) (replace b f)
replace (Variable a) f = f a
replace a _ = a