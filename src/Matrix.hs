{-# LANGUAGE FlexibleInstances #-}

module Matrix where

import MMP

data Matrix a = Matrix [[a]] | Scalar a deriving Show

instance (Eq a, MinMaxPlus a) => Eq (Matrix a) where
    (Matrix a) == (Matrix b) = a == b
    (Scalar a) == (Scalar b) = a == b
    (Scalar a) == (Matrix b) = getContext (scalar (Scalar a) (nrows (Matrix b))) == b
    (Matrix a) == (Scalar b) = a == getContext (scalar (Scalar b) (nrows (Matrix a)))

matrix :: Int -> Int -> ((Int, Int) -> a) -> Matrix a
matrix n m f = Matrix $ [[f (i, j) | j <- [1..m]] | i <- [1..n]]

nrows :: Matrix a -> Int
nrows (Matrix x) = length x

ncols :: Matrix a -> Int
ncols (Matrix x) = length $ head x

transpose' :: [[a]] -> [[a]]
transpose' ([]:_) = []
transpose' xs = map head xs : transpose' (map tail xs)

transpose :: Matrix a -> Matrix a
transpose (Matrix xs) = Matrix $ transpose' xs

getRow :: Matrix a -> Int -> [a]
getRow (Matrix m) i = m !! (i - 1)

getCol :: Matrix a -> Int -> [a]
getCol (Matrix m) i = transpose' m !! (i - 1)

getElem :: Matrix a -> Int -> Int -> a
getElem (Matrix a) i j = a !! (i - 1) !! (j - 1)

getContext :: Matrix a -> [[a]]
getContext (Matrix a) = a

applyM :: Matrix a -> (a -> a) -> Matrix a
applyM (Matrix a) f = 
    matrix (nrows (Matrix a)) (ncols (Matrix a)) (\(i, j) -> f (getElem (Matrix a) i j))

applyMIdx :: Matrix a -> (a -> (Int, Int) -> a) -> Matrix a
applyMIdx (Matrix a) f = 
    matrix (nrows (Matrix a)) (ncols (Matrix a)) (\(i, j) -> f (getElem (Matrix a) i j) (i, j))

scalar :: MinMaxPlus a => Matrix a -> Int -> Matrix a
scalar (Scalar a) ij = matrix ij ij f
    where
        f (i, j) | i == j = a
        f (i, j) | i /= j = zero

max' :: MinMaxPlus a => Matrix a -> Matrix a -> Matrix a
max' (Matrix a) (Matrix b) = matrix (nrows (Matrix a)) (ncols (Matrix a)) f
    where f (i, j) = (getElem (Matrix a) i j) @+ (getElem (Matrix b) i j)

max :: MinMaxPlus a => Matrix a -> Matrix a -> Matrix a
max (Scalar a) (Scalar b) = Scalar $ a @+ b
max (Matrix a) (Matrix b) = max' (Matrix a) (Matrix b)
max (Matrix a) (Scalar b) = max'  (Matrix a) (scalar (Scalar b) (nrows (Matrix a)))
max (Scalar a) (Matrix b) = max' (scalar (Scalar a) (nrows (Matrix b))) (Matrix b)

min' :: MinMaxPlus a => Matrix a -> Matrix a -> Matrix a
min' (Matrix a) (Matrix b) = matrix (nrows (Matrix a)) (ncols (Matrix a)) f
    where f (i, j) = (getElem (Matrix a) i j) @- (getElem (Matrix b) i j)

min :: MinMaxPlus a => Matrix a -> Matrix a -> Matrix a
min (Scalar a) (Scalar b) = Scalar $ a @- b
min (Matrix a) (Matrix b) = min' (Matrix a) (Matrix b)
min (Matrix a) (Scalar b) = min'  (Matrix a) (scalar (Scalar b) (nrows (Matrix a)))
min (Scalar a) (Matrix b) = min' (scalar (Scalar a) (nrows (Matrix b))) (Matrix b)

times' :: MinMaxPlus a => Matrix a -> Matrix a -> Matrix a
times' (Matrix a) (Matrix b) = matrix (nrows (Matrix a)) (ncols (Matrix b))
                               (\(i, j) -> f (getRow (Matrix a) i) (getCol (Matrix b) j))
    where
        f [x] [y] = x @* y
        f (x:xs) (y:ys) = (x @* y) @+ f xs ys

times :: MinMaxPlus a => Matrix a -> Matrix a -> Matrix a
times (Scalar a) (Scalar b) = Scalar $ a @* b
times (Matrix a) (Matrix b) = times' (Matrix a) (Matrix b)
times (Matrix a) (Scalar b) = times'  (Matrix a) (scalar (Scalar b) (nrows (Matrix a)))
times (Scalar a) (Matrix b) = times' (scalar (Scalar a) (nrows (Matrix b))) (Matrix b)

oslash' :: MinMaxPlus a => Matrix a -> Matrix a -> Matrix a
oslash' (Matrix a) (Matrix b) = matrix (nrows (Matrix a)) (ncols (Matrix b))
                               (\(i, j) -> f (getRow (Matrix a) i) (getCol (Matrix b) j))
    where
        f [x] [y] = x @* y
        f (x:xs) (y:ys) = (x @* y) @- f xs ys

oslash :: MinMaxPlus a => Matrix a -> Matrix a -> Matrix a
oslash (Scalar a) (Scalar b) = Scalar $ a @* b
oslash (Matrix a) (Matrix b) = oslash' (Matrix a) (Matrix b)
oslash (Matrix a) (Scalar b) = oslash'  (Matrix a) (scalar (Scalar b) (nrows (Matrix a)))
oslash (Scalar a) (Matrix b) = oslash' (scalar (Scalar a) (nrows (Matrix b))) (Matrix b)

(@@/) :: MinMaxPlus a => Matrix a -> Matrix a -> Matrix a
(@@/) = oslash
infixl 7 @@/

odot' :: MinMaxPlus a => Matrix a -> Matrix a -> Matrix a
odot' (Matrix a) (Matrix b) = matrix (nrows (Matrix a)) (ncols (Matrix b))
                               (\(i, j) -> f (getRow (Matrix a) i) (getCol (Matrix b) j))
    where
        f [x] [y] = x @/ y
        f (x:xs) (y:ys) = (x @/ y) @- f xs ys

odot :: MinMaxPlus a => Matrix a -> Matrix a -> Matrix a
odot (Scalar a) (Scalar b) = Scalar $ a @/ b
odot (Matrix a) (Matrix b) = odot' (Matrix a) (Matrix b)
odot (Matrix a) (Scalar b) = odot'  (Matrix a) (scalar (Scalar b) (nrows (Matrix a)))
odot (Scalar a) (Matrix b) = odot' (scalar (Scalar a) (nrows (Matrix b))) (Matrix b)

(@.) :: MinMaxPlus a => Matrix a -> Matrix a -> Matrix a
(@.) = odot
infixl 7 @.

instance MinMaxPlus a => MinMaxPlus (Matrix a) where
    zero = Scalar zero
    one = Scalar one
    max = Matrix.max
    min = Matrix.min
    plus = Matrix.times
    slash x y = transpose x @. y
