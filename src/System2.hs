module System2 where

import MMP
import Matrix
import Tropical
import Formula

{-
data Variables = Numerical Int | X5 | X7 | D1 | D2 | D3 | D4 | D5 | D6 | D7 deriving (Show, Eq)

trans :: Variables -> Formula (MMP Int) -> Formula (MMP Int) -> Formula (MMP Int)
trans X5 x5 _ = x5
trans X7 _ x7 = x7
trans (Numerical a) _ _ = Data (Tropical a)

trans' :: Formula Variables -> Formula (MMP Int)
trans' (Data a) = trans a (Data (Tropical 5)) (Data (Tropical 7))

fMax :: Matrix (Formula Variables)
fMax = Matrix
  [ [mzero, mzero, mzero, mzero, mzero, mzero, mzero]
  , [mzero, mzero, mzero, mzero, mzero, mzero, mzero]
  , [mzero, mzero, mzero, mzero, mzero, mzero, mzero]
  , [mzero, mzero, mzero, mzero, mzero, mzero, mzero]
  , [one, one, mzero, mzero, mzero, mzero, mzero]
  , [mzero, mzero, mzero, mzero, mzero, mzero, mzero]
  , [mzero, mzero, mzero, one, mzero, one, mzero]]

pMax :: Matrix (Formula Variables)
pMax = Matrix
  [ [Data D1, mzero, mzero, mzero, mzero, mzero, mzero]
  , [mzero, Data D2, mzero, mzero, mzero, mzero, mzero]
  , [mzero, mzero, Data D3, mzero, mzero, mzero, mzero]
  , [mzero, mzero, mzero, Data D4, mzero, mzero, mzero]
  , [mzero, mzero, mzero, mzero, Data D5, mzero, mzero]
  , [mzero, mzero, mzero, mzero, mzero, Data D6, mzero]
  , [mzero, mzero, mzero, mzero, mzero, mzero, Data D7]]

fMaxStar :: Matrix (Formula Variables)
fMaxStar = Scalar one @+ fMax @+ fMax @+^ 2 @+ fMax @+^ 3 @+ fMax @+^ 4 @+ fMax @+^ 5 @+ fMax @+^ 6

fpMaxStar :: Matrix (Formula Variables)
fpMaxStar = (Scalar one @+ pMax @* fMax @+ (pMax @* fMax) @+^ 2 @+ (pMax @* fMax) @+^ 3 @+ (pMax @* fMax) @+^ 4 @+ (pMax @* fMax) @+^ 5 @+ (pMax @* fMax) @+^ 6) @* pMax

fMin :: Matrix (Formula Variables)
fMin = Matrix
  [ [pzero, pzero, pzero, pzero, pzero, pzero, pzero]
  , [pzero, pzero, pzero, pzero, pzero, pzero, pzero]
  , [pzero, pzero, pzero, pzero, pzero, pzero, pzero]
  , [pzero, pzero, pzero, pzero, pzero, pzero, pzero]
  , [pzero, pzero, pzero, pzero, pzero, pzero, pzero]
  , [pzero, pzero, one, pzero, one, pzero, pzero]
  , [pzero, pzero, pzero, pzero, pzero, pzero, pzero]]

pMin :: Matrix (Formula Variables)
pMin = Matrix
  [ [Data D1, pzero, pzero, pzero, pzero, pzero, pzero]
  , [pzero, Data D2, pzero, pzero, pzero, pzero, pzero]
  , [pzero, pzero, Data D3, pzero, pzero, pzero, pzero]
  , [pzero, pzero, pzero, Data D4, pzero, pzero, pzero]
  , [pzero, pzero, pzero, pzero, Data D5, pzero, pzero]
  , [pzero, pzero, pzero, pzero, pzero, Data D6, pzero]
  , [pzero, pzero, pzero, pzero, pzero, pzero, Data D7]]

fMinStar :: Matrix (Formula Variables)
fMinStar = Scalar one @- fMin @- fMin @-^ 2 @- fMin @-^ 3 @- fMin @-^ 4 @- fMin @-^ 5 @- fMin @-^ 6

fpMinStar :: Matrix (Formula Variables)
fpMinStar = (Scalar one @- pMin @@/ fMin @- (pMin @@/ fMin) @-^ 2 @- (pMin @@/ fMin) @-^ 3 @- (pMin @@/ fMin) @-^ 4 @- (pMin @@/ fMin) @-^ 5 @- (pMin @@/ fMin) @-^ 6) @@/ pMin

pu :: Matrix (Formula Variables)
pu = transpose $ Matrix
  [[Data (Numerical 1), Data (Numerical 2), Data (Numerical 3), Data (Numerical 4), Data X5, pzero, Data X7]]

-- r1 = fMinStar @@/ pu
r1 :: Matrix (Formula Variables)
r1 = Matrix
  [[Data (Numerical 1)],[Data (Numerical 2)],[Data (Numerical 3)],[Data (Numerical 4)],[Data X5],[Min (Data (Numerical 3)) (Data X5)],[Data X7]]

r1' :: Matrix (Formula Variables)
r1' = Matrix
  [[Data (Numerical 1)],[Data (Numerical 2)],[Data (Numerical 3)],[Data (Numerical 4)],[mzero],[Min (Data (Numerical 3)) (Data X5)],[mzero]]

r2 :: Matrix (Formula Variables)
r2 = Matrix 
  [ [Plus (Data D1) (Data (Numerical 1))]
  , [Plus (Data D2) (Data (Numerical 2))]
  , [Plus (Data D3) (Data (Numerical 3))]
  , [Plus (Data D4) (Data (Numerical 4))]
  , [Plus (Data D5) (Data X5)]
  , [Min (Plus (Plus (Data D6) (Data D3)) (Data (Numerical 3))) (Plus (Plus (Data D6) (Data D5)) (Data X5))]
  , [Plus (Data D7) (Data X7)]]

r2' :: Matrix (Formula Variables)
r2' = Matrix 
  [ [Plus (Data D1) (Data (Numerical 1))]
  , [Plus (Data D2) (Data (Numerical 2))]
  , [Plus (Data D3) (Data (Numerical 3))]
  , [Plus (Data D4) (Data (Numerical 4))]
  , [mzero]
  , [Min (Plus (Plus (Data D6) (Data D3)) (Data (Numerical 3))) (Plus (Plus (Data D6) (Data D5)) (Data X5))]
  , [mzero]]

-}
{-
[[Plus (Data D1) (Data (Numerical 1))],
[Plus (Data D2) (Data (Numerical 2))],
[Plus (Data D3) (Data (Numerical 3))],
[Plus (Data D4) (Data (Numerical 4))],
[Max (Plus (Plus (Data D5) (Data D1)) (Data (Numerical 1))) (Plus (Plus (Data D5) (Data D2)) (Data (Numerical 2)))],
((d5 * d1) * u1) + ((d5 * d2) * u2)

[Plus (Data D6) (Min (Data (Numerical 3)) (Data X5))],
d6 * (u3 @- x5)
[Max (Plus (Plus (Data D7) (Data D4)) (Data (Numerical 4))) (Plus (Plus (Data D7) (Data D6)) (Min (Data (Numerical 3)) (Data X5)))]]

-------

[Plus (Data D1) (Data (Numerical 1))],
[Plus (Data D2) (Data (Numerical 2))],
[Plus (Data D3) (Data (Numerical 3))],
[Plus (Data D4) (Data (Numerical 4))],
[Max (Plus (Data D1) (Data (Numerical 1))) (Plus (Data D2) (Data (Numerical 2)))],
(d1 * u1) + (d3 * u2)
[Min (Plus (Plus (Data D6) (Data D3)) (Data (Numerical 3))) (Plus (Plus (Data D6) (Data D5)) (Data X5))],
((d6 * d3) * u3) @- ((d6 * d5) * x5)
[Max (Plus (Data D4) (Data (Numerical 4))) 
     (Min (Plus (Plus (Data D6) (Data D3)) (Data (Numerical 3))) (Plus (Plus (Data D6) (Data D5)) (Data X5)))]]
(d4 * u4) + (((d6 * d3) * u3) @- ((d6 * d5) * x5))
-}