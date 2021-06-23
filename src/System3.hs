module System3 where

import MMP
import Matrix
import Tropical
import Formula


data Variables = U Int | D Int | X Int deriving (Show, Eq)

fMax :: Matrix (Formula Variables)
fMax = Matrix
  [ [zero, zero, zero, zero, zero, zero, zero, zero, zero]
  , [zero, zero, zero, zero, zero, zero, zero, zero, zero]
  , [zero, zero, zero, zero, zero, zero, zero, zero, zero]
  , [zero, zero, zero, zero, zero, zero, zero, zero, zero]
  , [zero, zero, zero, zero, zero, zero, zero, zero, zero]
  , [one, one, zero, zero, zero, zero, zero, zero, zero]
  , [zero, zero, zero, zero, zero, zero, zero, zero, zero]
  , [zero, zero, zero, zero, zero, zero, zero, zero, zero]
  , [zero, zero, zero, zero, one, zero, zero, one, zero]]

fMaxStar :: Matrix (Formula Variables)
fMaxStar = one @+ fMax @+ fMax @+^ 2 @+ fMax @+^ 3 @+ fMax @+^ 4 @+ fMax @+^ 5 @+ fMax @+^ 6 @+ fMax @+^ 7 @+ fMax @+^ 8

fMin :: Matrix (Formula Variables)
fMin = Matrix
  [ [zero, zero, zero, zero, zero, zero, zero, zero, zero]
  , [zero, zero, zero, zero, zero, zero, zero, zero, zero]
  , [zero, zero, zero, zero, zero, zero, zero, zero, zero]
  , [zero, zero, zero, zero, zero, zero, zero, zero, zero]
  , [zero, zero, zero, zero, zero, zero, zero, zero, zero]
  , [zero, zero, zero, zero, zero, zero, zero, zero, zero]
  , [zero, zero, one, zero, zero, one, zero, zero, zero]
  , [zero, zero, zero, one, zero, zero, one, zero, zero]
  , [zero, zero, zero, zero, zero, zero, zero, zero, zero]]

pMin :: Matrix (Formula Variables)
pMin = Matrix
  [ [Variable (D 1), zero, zero, zero, zero, zero, zero, zero, zero]
  , [zero, Variable (D 2), zero, zero, zero, zero, zero, zero, zero]
  , [zero, zero, Variable (D 3), zero, zero, zero, zero, zero, zero]
  , [zero, zero, zero, Variable (D 4), zero, zero, zero, zero, zero]
  , [zero, zero, zero, zero, Variable (D 5), zero, zero, zero, zero]
  , [zero, zero, zero, zero, zero, Variable (D 6), zero, zero, zero]
  , [zero, zero, zero, zero, zero, zero, Variable (D 7), zero, zero]
  , [zero, zero, zero, zero, zero, zero, zero, Variable (D 8), zero]
  , [zero, zero, zero, zero, zero, zero, zero, zero, Variable (D 9)]]

pfMinStar :: Matrix (Formula Variables)
pfMinStar
  = one
  @- pMin @@/ fMin
  @- (pMin @@/ fMin) @-^ 2
  @- (pMin @@/ fMin) @-^ 3
  @- (pMin @@/ fMin) @-^ 4
  @- (pMin @@/ fMin) @-^ 5
  @- (pMin @@/ fMin) @-^ 6
  @- (pMin @@/ fMin) @-^ 7
  @- (pMin @@/ fMin) @-^ 8

u :: Matrix (Formula Variables)
u = transpose $ Matrix
  [[Variable (U 1), Variable (U 2), Variable (U 3), Variable (U 4), Variable (U 5), Variable (X 6), Variable (X 7), Variable (X 8), Variable (X 9)]]
     
x :: Matrix (Formula Variables)
x = Matrix
  [ [one, zero, zero, zero, zero, zero, zero, zero, zero]
  , [zero, one, zero, zero, zero, zero, zero, zero, zero]
  , [zero, zero, one, zero, zero, zero, zero, zero, zero]
  , [zero, zero, zero, one, zero, zero, zero, zero, zero]
  , [zero, zero, zero, zero, one, zero, zero, zero, zero]
  , [zero, zero, zero, zero, zero, zero, zero, zero, zero]
  , [zero, zero, zero, zero, zero, zero, one, zero, zero]
  , [zero, zero, zero, zero, zero, zero, zero, one, zero]
  , [zero, zero, zero, zero, zero, zero, zero, zero, zero]]

bMin :: Matrix (Formula Variables)
bMin = pfMinStar @@/ pMin @@/ u

bMin' :: Matrix (Formula Variables)
bMin' = applyMIdx bMin (\formula (i, j) -> replace formula (f i))
  where
    f :: Int -> Variables -> Formula Variables
    f i x | i == 6 && x == X 6 = zero
    f i x | i == 7 && x == X 7 = zero 
    f i x | i == 8 && x == X 8 = zero 
    f i x | i == 9 && x == X 9 = zero
    f _ x = Variable x

r :: Matrix (Formula Variables)
r = fMaxStar @* x @* bMin'

{-
[
[Plus (Variable (D 1)) (Variable (U 1))],
[Plus (Variable (D 2)) (Variable (U 2))],
[Plus (Variable (D 3)) (Variable (U 3))],
[Plus (Variable (D 4)) (Variable (U 4))],
[Plus (Variable (D 5)) (Variable (U 5))],[Plus (Variable (D 6)) (Variable (X 6))],
[Min (Plus (Plus (Variable (D 7)) (Variable (D 3))) (Variable (U 3))) (Min (Plus (Plus (Variable (D 7)) (Variable (D 6))) (Variable (X 6))) (Plus (Variable (D 7)) (Variable (X 7))))],
[Min (Plus (Plus (Variable (D 8)) (Variable (D 4))) (Variable (U 4))) (Min (Plus (Plus (Variable (D 8)) (Variable (D 7))) (Variable (X 7))) (Plus (Variable (D 8)) (Variable (X 8))))],
[Plus (Variable (D 9)) (Variable (X 9))]
]

[
[Plus (Variable (D 1)) (Variable (U 1))],
[Plus (Variable (D 2)) (Variable (U 2))],
[Plus (Variable (D 3)) (Variable (U 3))],
[Plus (Variable (D 4)) (Variable (U 4))],
[Plus (Variable (D 5)) (Variable (U 5))],
[Max (Plus (Variable (D 1)) (Variable (U 1))) (Plus (Variable (D 2)) (Variable (U 2)))],
[Min (Plus (Plus (Variable (D 7)) (Variable (D 3))) (Variable (U 3))) (Min (Plus (Plus (Variable (D 7)) (Variable (D 6))) (Variable (X 6))) (Plus (Variable (D 7)) MInfinity))],
[Min (Plus (Plus (Variable (D 8)) (Variable (D 4))) (Variable (U 4))) (Min (Plus (Plus (Variable (D 8)) (Variable (D 7))) (Variable (X 7))) (Plus (Variable (D 8)) MInfinity))],
[Max (Plus (Variable (D 5)) (Variable (U 5))) (Min (Plus (Plus (Variable (D 8)) (Variable (D 4))) (Variable (U 4))) (Min (Plus (Plus (Variable (D 8)) (Variable (D 7))) (Variable (X 7))) (Plus (Variable (D 8)) MInfinity)))]
]
-}
