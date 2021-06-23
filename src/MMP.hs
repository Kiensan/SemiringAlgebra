module MMP where

import Prelude as Pre

class MinMaxPlus a where
  zero :: a
  one :: a
  max :: a -> a -> a
  min :: a -> a -> a
  plus :: a -> a -> a
  slash :: a -> a -> a

(@+) :: MinMaxPlus a => a -> a -> a
(@+) = MMP.max
infixl 6 @+

(@-) :: MinMaxPlus a => a -> a -> a
(@-) = MMP.min
infixl 6 @-

(@*) :: MinMaxPlus a => a -> a -> a
(@*) = plus
infixl 7 @*

(@/) :: MinMaxPlus a => a -> a -> a
(@/) = slash
infixl 7 @/

(@+^) :: MinMaxPlus a => a -> Int -> a
(@+^) _ n | n == 0 = one
(@+^) a n | n == 1 = a
(@+^) a n | n >= 2 = a @* (a @+^ (n - 1))
infixr 8 @+^

(@-^) :: MinMaxPlus a => a -> Int -> a
(@-^) _ n | n == 0 = one
(@-^) a n | n == 1 = a
(@-^) a n | n >= 2 = a @* (a @-^ (n - 1))
infixr 8 @-^

pkleene' :: (Eq a, MinMaxPlus a) => a -> Int -> a -> a
pkleene' a n x = if a @+^ n == zero then x else pkleene' a (n + 1) (x @+ (a @+^ n))

pkleene :: (Eq a, MinMaxPlus a) => a -> a
pkleene a = pkleene' a 1 one

dkleene' :: (Eq a, MinMaxPlus a) => a -> Int -> a -> a
dkleene' a n x = if a @-^ n == zero then x else dkleene' a (n + 1) (x @- (a @-^ n))

dkleene :: (Eq a, MinMaxPlus a) => a -> a
dkleene a = dkleene' a 1 one
