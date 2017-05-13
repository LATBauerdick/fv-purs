module Data.Cov
  {-- ( Cov (..) --}
  {-- , Vec, Vec3, Vec5 --}
  {-- , Jacos, Jac53, Jac --}
  {-- , Dim3, Dim5, Dim53 --}
  {-- , class Mat, val --}
  {-- , testCov --}
  {-- ) --}
    where

import Prelude
import Data.Array
  ( replicate, unsafeIndex, zipWith, length, singleton, foldl, range
  ) as A
import Data.Array.Partial ( head ) as AP
import Partial.Unsafe ( unsafePartial )
import Data.Maybe (Maybe (..), fromJust)
import Control.MonadZero (guard)
import Math ( abs )
import Data.Matrix
  ( Matrix (..)
  , multStd, tr, scalar, scaleDiag
  , fromArray, fromArray2, zero_, identity, toArray
  ) as M
import Stuff

newtype Dim3 = Dim3 Int
newtype Dim4 = Dim4 Int
newtype Dim5 = Dim5 Int

newtype Cov a = Cov { v :: Array Number }
newtype Jac a b = Jac { v :: Array Number }
newtype Vec a = Vec { v :: Array Number }
type Cov3 = Cov Dim3
type Cov4 = Cov Dim4
type Cov5 = Cov Dim5
type Jac53 = Jac Dim5 Dim3
type Jac43 = Jac Dim4 Dim3
type Jac34 = Jac Dim3 Dim4
type Jac35 = Jac Dim3 Dim5
type Jac55 = Jac Dim5 Dim5
type Vec3 = Vec Dim3
type Vec4 = Vec Dim4
type Vec5 = Vec Dim5
type Jacs = {aa :: Jac53, bb :: Jac53, h0 :: Vec5}

class Mat a where
  val :: a -> Array Number
  fromArray :: Array Number -> a
  toArray :: a -> Array Number
  toMatrix :: a -> M.Matrix Number
instance matCov3 :: Mat (Cov Dim3) where
  val (Cov {v}) = v
  fromArray a | A.length a == 6 = Cov {v: a}
              | A.length a == 9 = Cov {v: a'} where
    a11 = unsafePartial $ A.unsafeIndex a 0
    a12 = unsafePartial $ A.unsafeIndex a 1
    a13 = unsafePartial $ A.unsafeIndex a 2
    a22 = unsafePartial $ A.unsafeIndex a 4
    a23 = unsafePartial $ A.unsafeIndex a 5
    a33 = unsafePartial $ A.unsafeIndex a 8
    a' = [a11,a12,a13,a22,a23,a33]
              | otherwise = error $ "Cov3 fromArray: wrong input array length "
                                  <> show (A.length a)
  toArray (Cov {v}) = vv where
    a11 = unsafePartial $ A.unsafeIndex v 0
    a12 = unsafePartial $ A.unsafeIndex v 1
    a13 = unsafePartial $ A.unsafeIndex v 2
    a22 = unsafePartial $ A.unsafeIndex v 3
    a23 = unsafePartial $ A.unsafeIndex v 4
    a33 = unsafePartial $ A.unsafeIndex v 5
    vv = [a11, a12, a13, a12, a22, a23, a13, a23, a33]
  toMatrix c = M.fromArray2 3 3 $ toArray c
instance matCov4 :: Mat (Cov Dim4) where
  val (Cov {v}) = v
  fromArray a | A.length a == 10 = Cov {v: a}
              | A.length a == 16 = Cov {v: a'} where
    a11 = unsafePartial $ A.unsafeIndex a 0
    a12 = unsafePartial $ A.unsafeIndex a 1
    a13 = unsafePartial $ A.unsafeIndex a 2
    a14 = unsafePartial $ A.unsafeIndex a 3
    a22 = unsafePartial $ A.unsafeIndex a 5
    a23 = unsafePartial $ A.unsafeIndex a 6
    a24 = unsafePartial $ A.unsafeIndex a 7
    a33 = unsafePartial $ A.unsafeIndex a 10
    a34 = unsafePartial $ A.unsafeIndex a 11
    a44 = unsafePartial $ A.unsafeIndex a 15
    a' = [a11,a12,a13,a14,a22,a23,a24,a33,a34,a44]
              | otherwise = error $ "Cov4 fromArray: wrong input array length "
                                  <> show (A.length a)
  toArray (Cov {v}) = vv where
    a11 = unsafePartial $ A.unsafeIndex v 0
    a12 = unsafePartial $ A.unsafeIndex v 1
    a13 = unsafePartial $ A.unsafeIndex v 2
    a14 = unsafePartial $ A.unsafeIndex v 3
    a22 = unsafePartial $ A.unsafeIndex v 4
    a23 = unsafePartial $ A.unsafeIndex v 5
    a24 = unsafePartial $ A.unsafeIndex v 6
    a33 = unsafePartial $ A.unsafeIndex v 7
    a34 = unsafePartial $ A.unsafeIndex v 8
    a44 = unsafePartial $ A.unsafeIndex v 9
    vv = [a11,a12,a13,a14,a12,a22,a23,a24,a13,a23,a33,a34,a14,a24,a34,a44]
  toMatrix c = M.fromArray2 4 4 $ toArray c
instance matCov5 :: Mat (Cov Dim5) where
  val (Cov {v}) = v
  fromArray a | A.length a == 15 = Cov {v: a}
              | A.length a == 25 = Cov {v: a'} where
    a11 = unsafePartial $ A.unsafeIndex a 0
    a12 = unsafePartial $ A.unsafeIndex a 1
    a13 = unsafePartial $ A.unsafeIndex a 2
    a14 = unsafePartial $ A.unsafeIndex a 3
    a15 = unsafePartial $ A.unsafeIndex a 4
    a22 = unsafePartial $ A.unsafeIndex a 6
    a23 = unsafePartial $ A.unsafeIndex a 7
    a24 = unsafePartial $ A.unsafeIndex a 8
    a25 = unsafePartial $ A.unsafeIndex a 9
    a33 = unsafePartial $ A.unsafeIndex a 12
    a34 = unsafePartial $ A.unsafeIndex a 13
    a35 = unsafePartial $ A.unsafeIndex a 14
    a44 = unsafePartial $ A.unsafeIndex a 18
    a45 = unsafePartial $ A.unsafeIndex a 19
    a55 = unsafePartial $ A.unsafeIndex a 24
    a' = [a11,a12,a13,a14,a15,a22,a23,a24,a25,a33,a34,a35,a44,a45,a55]
              | otherwise = error "Cov5 fromArray: wrong input array length"
  toArray (Cov {v}) = vv where
    a11 = unsafePartial $ A.unsafeIndex v 0
    a12 = unsafePartial $ A.unsafeIndex v 1
    a13 = unsafePartial $ A.unsafeIndex v 2
    a14 = unsafePartial $ A.unsafeIndex v 3
    a15 = unsafePartial $ A.unsafeIndex v 4
    a22 = unsafePartial $ A.unsafeIndex v 5
    a23 = unsafePartial $ A.unsafeIndex v 6
    a24 = unsafePartial $ A.unsafeIndex v 7
    a25 = unsafePartial $ A.unsafeIndex v 8
    a33 = unsafePartial $ A.unsafeIndex v 9
    a34 = unsafePartial $ A.unsafeIndex v 10
    a35 = unsafePartial $ A.unsafeIndex v 11
    a44 = unsafePartial $ A.unsafeIndex v 12
    a45 = unsafePartial $ A.unsafeIndex v 13
    a55 = unsafePartial $ A.unsafeIndex v 14
    vv = [a11, a12, a13, a14, a15, a12, a22, a23, a24, a25
         ,a13, a23, a33, a34, a35, a14, a24, a34, a44, a45
         ,a15, a25, a35, a45, a55]
  toMatrix c = M.fromArray2 5 5 $ toArray c
instance matVec3 :: Mat (Vec Dim3) where
  val (Vec {v}) = v
  fromArray a | A.length a /= 3 =
                  error "Vec3 fromArray: wrong input array length"
              | otherwise = Vec {v: a}
  toArray (Vec {v}) = v
  toMatrix (Vec {v}) = M.fromArray 3 v
instance matVec4 :: Mat (Vec Dim4) where
  val (Vec {v}) = v
  fromArray a | A.length a /= 4 =
                  error "Vec4 fromArray: wrong input array length"
              | otherwise = Vec {v: a}
  toArray (Vec {v}) = v
  toMatrix (Vec {v}) = M.fromArray 4 v
instance matVec5 :: Mat (Vec Dim5) where
  val (Vec {v}) = v
  fromArray a | A.length a /= 5 =
                  error "Vec5 fromArray: wrong input array length"
              | otherwise = Vec {v: a}
  toArray (Vec {v}) = v
  toMatrix (Vec {v}) = M.fromArray 5 v
instance matJac53 :: Mat (Jac Dim5 Dim3) where
  val (Jac {v}) = v
  fromArray a | A.length a /= 15 =
                  error "Jac53 fromArray: wrong input array length"
              | otherwise = Jac {v: a}
  toArray (Jac {v}) = v
  toMatrix (Jac {v}) = M.fromArray2 5 3 v
instance matJac43 :: Mat (Jac Dim4 Dim3) where
  val (Jac {v}) = v
  fromArray a | A.length a /= 12 =
                  error "Jac43 fromArray: wrong input array length"
              | otherwise = Jac {v: a}
  toArray (Jac {v}) = v
  toMatrix (Jac {v}) = M.fromArray2 3 4 v
instance matJac34 :: Mat (Jac Dim3 Dim4) where
  val (Jac {v}) = v
  fromArray a | A.length a /= 12 =
                  error "Jac34 fromArray: wrong input array length"
              | otherwise = Jac {v: a}
  toArray (Jac {v}) = v
  toMatrix (Jac {v}) = M.fromArray2 3 4 v
instance matJac35 :: Mat (Jac Dim3 Dim5) where
  val (Jac {v}) = v
  fromArray a | A.length a /= 15 =
                  error "Jac35 fromArray: wrong input array length"
              | otherwise = Jac {v: a}
  toArray (Jac {v}) = v
  toMatrix (Jac {v}) = M.fromArray2 3 5 v
instance matJac55 :: Mat (Jac Dim5 Dim5) where
  val (Jac {v}) = v
  fromArray a | A.length a /= 25 =
                  error "Jac55 fromArray: wrong input array length"
              | otherwise = Jac {v: a}
  toArray (Jac {v}) = v
  toMatrix (Jac {v}) = M.fromArray2 5 5 v
class SymMat a where
  inv :: a -> a
  invMaybe :: a -> Maybe a
  det :: a -> Number
  diag :: a -> Array Number
instance symMatCov3 :: SymMat (Cov Dim3) where
  inv m = unsafePartial $ fromJust (invMaybe m)
  invMaybe (Cov {v}) = do
    let
        a11 = unsafePartial $ A.unsafeIndex v 0
        a12 = unsafePartial $ A.unsafeIndex v 1
        a13 = unsafePartial $ A.unsafeIndex v 2
        a22 = unsafePartial $ A.unsafeIndex v 3
        a23 = unsafePartial $ A.unsafeIndex v 4
        a33 = unsafePartial $ A.unsafeIndex v 5
        det = (a33*a12*a12 - 2.0*a13*a23*a12 + a13*a13*a22
            +a11*(a23*a23 - a22*a33))
    guard $ (abs det) > 1.0e-50
    let
        b11 = (a23*a23 - a22*a33)/det
        b12 = (a12*a33 - a13*a23)/det
        b13 = (a13*a22 - a12*a23)/det
        b22 = (a13*a13 - a11*a33)/det
        b23 = (a11*a23 - a12*a13)/det
        b33 = (a12*a12 - a11*a22)/det
        v' = [b11,b12,b13,b22,b23,b33]
    pure $ fromArray v'-- `debug` ("------------>>>>>" <> show v')
  det a = 1.0
  diag (Cov {v}) = a where
    a11 = unsafePartial $ A.unsafeIndex v 0
    a22 = unsafePartial $ A.unsafeIndex v 3
    a33 = unsafePartial $ A.unsafeIndex v 5
    a = [a11,a22,a33]
instance symMatCov4 :: SymMat (Cov Dim4) where
  inv m = unsafePartial $ fromJust (invMaybe m)
  invMaybe (Cov {v}) = do
    let
        a = unsafePartial $ A.unsafeIndex v 0
        b = unsafePartial $ A.unsafeIndex v 1
        c = unsafePartial $ A.unsafeIndex v 2
        d = unsafePartial $ A.unsafeIndex v 3
        e = unsafePartial $ A.unsafeIndex v 4
        f = unsafePartial $ A.unsafeIndex v 5
        g = unsafePartial $ A.unsafeIndex v 6
        h = unsafePartial $ A.unsafeIndex v 7
        i = unsafePartial $ A.unsafeIndex v 8
        j = unsafePartial $ A.unsafeIndex v 9
    let det = (a*e*h*j - a*e*i*i - a*f*f*j + 2.0*a*f*g*i - a*g*g*h
          - b*b*h*j + b*b*i*i - 2.0*d*(b*f*i - b*g*h - c*e*i + c*f*g)
          + b*c*(2.0*f*j - 2.0*g*i) + c*c*(g*g - e*j) + d*d*(f*f - e*h))
    guard $ (abs det) > 1.0e-50
    let a' = (-j*f*f + 2.0*g*i*f - e*i*i - g*g*h + e*h*j)/det
        b' = (b*i*i - d*f*i - c*g*i + d*g*h + c*f*j - b*h*j)/det
        c' = (c*g*g - d*f*g - b*i*g + d*e*i - c*e*j + b*f*j)/det
        d' = (d*f*f - c*g*f - b*i*f - d*e*h + b*g*h + c*e*i)/det
        e' = (-j*c*c + 2.0*d*i*c - a*i*i - d*d*h + a*h*j)/det
        f' = (f*d*d - c*g*d - b*i*d + a*g*i + b*c*j - a*f*j)/det
        g' = (g*c*c - d*f*c - b*i*c + b*d*h - a*g*h + a*f*i)/det
        h' = (-j*b*b + 2.0*d*g*b - a*g*g - d*d*e + a*e*j)/det
        i' = (i*b*b - d*f*b - c*g*b + c*d*e + a*f*g - a*e*i)/det
        j' = (-h*b*b + 2.0*c*f*b - a*f*f - c*c*e + a*e*h)/det
        v' = [a',b',c',d',e',f',g',h',i',j']
    pure $ fromArray v' `debug` ("------------>>>>>" <> show v')
  det (Cov {v}) = d' where
    a = unsafePartial $ A.unsafeIndex v 0
    b = unsafePartial $ A.unsafeIndex v 1
    c = unsafePartial $ A.unsafeIndex v 2
    d = unsafePartial $ A.unsafeIndex v 3
    e = unsafePartial $ A.unsafeIndex v 4
    f = unsafePartial $ A.unsafeIndex v 5
    g = unsafePartial $ A.unsafeIndex v 6
    h = unsafePartial $ A.unsafeIndex v 7
    i = unsafePartial $ A.unsafeIndex v 8
    j = unsafePartial $ A.unsafeIndex v 9
    d' = (a*e*h*j - a*e*i*i - a*f*f*j + 2.0*a*f*g*i - a*g*g*h
          - b*b*h*j + b*b*i*i - 2.0*d*(b*f*i - b*g*h - c*e*i + c*f*g)
          + b*c*(2.0*f*j - 2.0*g*i) + c*c*(g*g - e*j) + d*d*(f*f - e*h))
  diag (Cov {v}) = a where
    a11 = unsafePartial $ A.unsafeIndex v 0
    a22 = unsafePartial $ A.unsafeIndex v 4
    a33 = unsafePartial $ A.unsafeIndex v 7
    a44 = unsafePartial $ A.unsafeIndex v 9
    a = [a11,a22,a33,a44]
instance symMatCov5 :: SymMat (Cov Dim5) where
  inv m = unsafePartial $ fromJust (invMaybe m)
  invMaybe (Cov {v}) = do
    let
        a = unsafePartial $ A.unsafeIndex v 0
        b = unsafePartial $ A.unsafeIndex v 1
        c = unsafePartial $ A.unsafeIndex v 2
        d = unsafePartial $ A.unsafeIndex v 3
        e = unsafePartial $ A.unsafeIndex v 4
        f = unsafePartial $ A.unsafeIndex v 5
        g = unsafePartial $ A.unsafeIndex v 6
        h = unsafePartial $ A.unsafeIndex v 7
        i = unsafePartial $ A.unsafeIndex v 8
        j = unsafePartial $ A.unsafeIndex v 9
        k = unsafePartial $ A.unsafeIndex v 10
        l = unsafePartial $ A.unsafeIndex v 11
        m = unsafePartial $ A.unsafeIndex v 12
        n = unsafePartial $ A.unsafeIndex v 13
        o = unsafePartial $ A.unsafeIndex v 14
        det = a*f*j*m*o - a*f*j*n*n - a*f*k*k*o + 2.0*a*f*k*l*n - a*f*l*l*m 
          - a*g*g*m*o + a*g*g*n*n + 2.0*a*g*h*k*o - 2.0*a*g*h*l*n 
          - 2.0*a*g*i*k*n + 2.0*a*g*i*l*m - a*h*h*j*o + a*h*h*l*l 
          + 2.0*a*h*i*j*n - 2.0*a*h*i*k*l - a*i*i*j*m + a*i*i*k*k - b*b*j*m*o 
          + b*b*j*n*n + b*b*k*k*o - 2.0*b*b*k*l*n + b*b*l*l*m + 2.0*b*c*g*m*o 
          - 2.0*b*c*g*n*n - 2.0*b*c*h*k*o + 2.0*b*c*h*l*n + 2.0*b*c*i*k*n 
          - 2.0*b*c*i*l*m - 2.0*b*d*g*k*o + 2.0*b*d*g*l*n + 2.0*b*d*h*j*o 
          - 2.0*b*d*h*l*l - 2.0*b*d*i*j*n + 2.0*b*d*i*k*l + 2.0*b*e*g*k*n 
          - 2.0*b*e*g*l*m - 2.0*b*e*h*j*n + 2.0*b*e*h*k*l + 2.0*b*e*i*j*m 
          - 2.0*b*e*i*k*k - c*c*f*m*o + c*c*f*n*n + c*c*h*h*o - 2.0*c*c*h*i*n 
          + c*c*i*i*m + 2.0*c*d*f*k*o - 2.0*c*d*f*l*n - 2.0*c*d*g*h*o 
          + 2.0*c*d*g*i*n + 2.0*c*d*h*i*l - 2.0*c*d*i*i*k - 2.0*c*e*f*k*n 
          + 2.0*c*e*f*l*m + 2.0*c*e*g*h*n - 2.0*c*e*g*i*m - 2.0*c*e*h*h*l 
          + 2.0*c*e*h*i*k - d*d*f*j*o + d*d*f*l*l + d*d*g*g*o 
          - 2.0*d*d*g*i*l + d*d*i*i*j + 2.0*d*e*f*j*n - 2.0*d*e*f*k*l 
          - 2.0*d*e*g*g*n + 2.0*d*e*g*h*l + 2.0*d*e*g*i*k - 2.0*d*e*h*i*j 
          - e*e*f*j*m + e*e*f*k*k + e*e*g*g*m - 2.0*e*e*g*h*k + e*e*h*h*j
    guard $ (abs det) > 1.0e-50
    let
        a' = (i*i*k*k - 2.0*h*i*k*l + h*h*l*l - i*i*j*m + 2.0*g*i*l*m 
            - f*l*l*m + 2.0*h*i*j*n - 2.0*g*i*k*n - 2.0*g*h*l*n + 2.0*f*k*l*n 
            + g*g*n*n - f*j*n*n - h*h*j*o + 2.0*g*h*k*o - f*k*k*o 
            - g*g*m*o + f*j*m*o)/det
        b' = (-e*i*k*k + e*h*k*l + d*i*k*l - d*h*l*l + e*i*j*m - e*g*l*m 
          - c*i*l*m + b*l*l*m - e*h*j*n - d*i*j*n + e*g*k*n + c*i*k*n 
          + d*g*l*n + c*h*l*n - 2.0*b*k*l*n - c*g*n*n + b*j*n*n + d*h*j*o 
          - d*g*k*o - c*h*k*o + b*k*k*o + c*g*m*o - b*j*m*o)/det
        c' = (e*h*i*k - d*i*i*k - e*h*h*l + d*h*i*l - e*g*i*m + c*i*i*m 
          + e*f*l*m - b*i*l*m + e*g*h*n + d*g*i*n - 2.0*c*h*i*n - e*f*k*n 
          + b*i*k*n - d*f*l*n + b*h*l*n + c*f*n*n - b*g*n*n - d*g*h*o 
          + c*h*h*o + d*f*k*o - b*h*k*o - c*f*m*o + b*g*m*o)/det
        d' = (-e*h*i*j + d*i*i*j + e*g*i*k - c*i*i*k + e*g*h*l - 2.0*d*g*i*l 
          + c*h*i*l - e*f*k*l + b*i*k*l + d*f*l*l - b*h*l*l - e*g*g*n 
          + c*g*i*n + e*f*j*n - b*i*j*n - c*f*l*n + b*g*l*n + d*g*g*o 
          - c*g*h*o - d*f*j*o + b*h*j*o + c*f*k*o - b*g*k*o)/det
        e' = (e*h*h*j - d*h*i*j - 2.0*e*g*h*k + d*g*i*k + c*h*i*k + e*f*k*k 
          - b*i*k*k + d*g*h*l - c*h*h*l - d*f*k*l + b*h*k*l + e*g*g*m 
          - c*g*i*m - e*f*j*m + b*i*j*m + c*f*l*m - b*g*l*m - d*g*g*n 
          + c*g*h*n + d*f*j*n - b*h*j*n - c*f*k*n + b*g*k*n)/det
        f' = (e*e*k*k - 2.0*d*e*k*l + d*d*l*l - e*e*j*m + 2.0*c*e*l*m - a*l*l*m 
          + 2.0*d*e*j*n - 2.0*c*e*k*n - 2.0*c*d*l*n + 2.0*a*k*l*n + c*c*n*n 
          - a*j*n*n - d*d*j*o + 2.0*c*d*k*o - a*k*k*o - c*c*m*o + a*j*m*o)/det
        g' = (-e*e*h*k + d*e*i*k + d*e*h*l - d*d*i*l + e*e*g*m - c*e*i*m 
          - b*e*l*m + a*i*l*m - 2.0*d*e*g*n + c*e*h*n + c*d*i*n + b*e*k*n 
          - a*i*k*n + b*d*l*n - a*h*l*n - b*c*n*n + a*g*n*n + d*d*g*o 
          - c*d*h*o - b*d*k*o + a*h*k*o + b*c*m*o - a*g*m*o)/det
        h' = (e*e*h*j - d*e*i*j - e*e*g*k + c*e*i*k + d*e*g*l - 2.0*c*e*h*l 
          + c*d*i*l + b*e*k*l - a*i*k*l - b*d*l*l + a*h*l*l + c*e*g*n 
          - c*c*i*n - b*e*j*n + a*i*j*n + b*c*l*n - a*g*l*n - c*d*g*o 
          + c*c*h*o + b*d*j*o - a*h*j*o - b*c*k*o + a*g*k*o)/det
        i' = (-d*e*h*j + d*d*i*j + d*e*g*k + c*e*h*k - 2.0*c*d*i*k 
          - b*e*k*k + a*i*k*k - d*d*g*l + c*d*h*l + b*d*k*l - a*h*k*l 
          - c*e*g*m + c*c*i*m + b*e*j*m - a*i*j*m - b*c*l*m + a*g*l*m 
          + c*d*g*n - c*c*h*n - b*d*j*n + a*h*j*n + b*c*k*n - a*g*k*n)/det
        j' = (e*e*h*h - 2.0*d*e*h*i + d*d*i*i - e*e*f*m + 2.0*b*e*i*m - a*i*i*m 
          + 2.0*d*e*f*n - 2.0*b*e*h*n - 2.0*b*d*i*n + 2.0*a*h*i*n  + b*b*n*n 
          - a*f*n*n - d*d*f*o + 2.0*b*d*h*o - a*h*h*o - b*b*m*o + a*f*m*o)/det
        k' = (-e*e*g*h + d*e*g*i + c*e*h*i - c*d*i*i + e*e*f*k - 2.0*b*e*i*k 
          + a*i*i*k - d*e*f*l + b*e*h*l + b*d*i*l - a*h*i*l - c*e*f*n 
          + b*e*g*n + b*c*i*n - a*g*i*n - b*b*l*n + a*f*l*n + c*d*f*o 
          - b*d*g*o - b*c*h*o + a*g*h*o + b*b*k*o - a*f*k*o)/det
        l' = (d*e*g*h - c*e*h*h - d*d*g*i + c*d*h*i - d*e*f*k + b*e*h*k 
          + b*d*i*k - a*h*i*k + d*d*f*l - 2.0*b*d*h*l + a*h*h*l + c*e*f*m 
          - b*e*g*m - b*c*i*m + a*g*i*m + b*b*l*m - a*f*l*m - c*d*f*n 
          + b*d*g*n + b*c*h*n - a*g*h*n - b*b*k*n + a*f*k*n)/det
        m' = (e*e*g*g - 2.0*c*e*g*i + c*c*i*i - e*e*f*j + 2.0*b*e*i*j - a*i*i*j 
          + 2.0*c*e*f*l - 2.0*b*e*g*l - 2.0*b*c*i*l + 2.0*a*g*i*l + b*b*l*l 
          - a*f*l*l - c*c*f*o + 2.0*b*c*g*o - a*g*g*o - b*b*j*o + a*f*j*o)/det
        n' = (-d*e*g*g + c*e*g*h + c*d*g*i - c*c*h*i + d*e*f*j - b*e*h*j 
          - b*d*i*j + a*h*i*j - c*e*f*k + b*e*g*k + b*c*i*k - a*g*i*k 
          - c*d*f*l + b*d*g*l + b*c*h*l - a*g*h*l - b*b*k*l + a*f*k*l 
          + c*c*f*n - 2.0*b*c*g*n + a*g*g*n + b*b*j*n - a*f*j*n)/det
        o' = (d*d*g*g - 2.0*c*d*g*h + c*c*h*h - d*d*f*j + 2.0*b*d*h*j - a*h*h*j 
          + 2.0*c*d*f*k - 2.0*b*d*g*k - 2.0*b*c*h*k + 2.0*a*g*h*k + b*b*k*k 
          - a*f*k*k - c*c*f*m + 2.0*b*c*g*m - a*g*g*m - b*b*j*m + a*f*j*m)/det
        v' = [a',b',c',d',e',f',g',h',i',j',k',l',m',n',o']
    pure $ fromArray v' `debug` ("------------>>>>>" <> show v')
  det (Cov {v}) = d' where
    a = unsafePartial $ A.unsafeIndex v 0
    b = unsafePartial $ A.unsafeIndex v 1
    c = unsafePartial $ A.unsafeIndex v 2
    d = unsafePartial $ A.unsafeIndex v 3
    e = unsafePartial $ A.unsafeIndex v 4
    f = unsafePartial $ A.unsafeIndex v 5
    g = unsafePartial $ A.unsafeIndex v 6
    h = unsafePartial $ A.unsafeIndex v 7
    i = unsafePartial $ A.unsafeIndex v 8
    j = unsafePartial $ A.unsafeIndex v 9
    k = unsafePartial $ A.unsafeIndex v 10
    l = unsafePartial $ A.unsafeIndex v 11
    m = unsafePartial $ A.unsafeIndex v 12
    n = unsafePartial $ A.unsafeIndex v 13
    o = unsafePartial $ A.unsafeIndex v 14
    d' = a*f*j*m*o - a*f*j*n*n - a*f*k*k*o + 2.0*a*f*k*l*n - a*f*l*l*m 
      - a*g*g*m*o + a*g*g*n*n + 2.0*a*g*h*k*o - 2.0*a*g*h*l*n - 2.0*a*g*i*k*n 
      + 2.0*a*g*i*l*m - a*h*h*j*o + a*h*h*l*l + 2.0*a*h*i*j*n - 2.0*a*h*i*k*l 
      - a*i*i*j*m + a*i*i*k*k - b*b*j*m*o + b*b*j*n*n + b*b*k*k*o
      - 2.0*b*b*k*l*n + b*b*l*l*m + 2.0*b*c*g*m*o - 2.0*b*c*g*n*n - 2.0*b*c*h*k*o 
      + 2.0*b*c*h*l*n + 2.0*b*c*i*k*n - 2.0*b*c*i*l*m - 2.0*b*d*g*k*o + 2.0*b*d*g*l*n 
      + 2.0*b*d*h*j*o - 2.0*b*d*h*l*l - 2.0*b*d*i*j*n + 2.0*b*d*i*k*l + 2.0*b*e*g*k*n 
      - 2.0*b*e*g*l*m - 2.0*b*e*h*j*n + 2.0*b*e*h*k*l + 2.0*b*e*i*j*m - 2.0*b*e*i*k*k 
      - c*c*f*m*o + c*c*f*n*n + c*c*h*h*o - 2.0*c*c*h*i*n + c*c*i*i*m 
      + 2.0*c*d*f*k*o - 2.0*c*d*f*l*n - 2.0*c*d*g*h*o + 2.0*c*d*g*i*n + 2.0*c*d*h*i*l 
      - 2.0*c*d*i*i*k - 2.0*c*e*f*k*n + 2.0*c*e*f*l*m + 2.0*c*e*g*h*n - 2.0*c*e*g*i*m 
      - 2.0*c*e*h*h*l + 2.0*c*e*h*i*k - d*d*f*j*o + d*d*f*l*l + d*d*g*g*o 
      - 2.0*d*d*g*i*l + d*d*i*i*j + 2.0*d*e*f*j*n - 2.0*d*e*f*k*l - 2.0*d*e*g*g*n 
      + 2.0*d*e*g*h*l + 2.0*d*e*g*i*k - 2.0*d*e*h*i*j - e*e*f*j*m + e*e*f*k*k 
      + e*e*g*g*m - 2.0*e*e*g*h*k + e*e*h*h*j
  diag (Cov {v}) = a where
    a11 = unsafePartial $ A.unsafeIndex v 0
    a22 = unsafePartial $ A.unsafeIndex v 5
    a33 = unsafePartial $ A.unsafeIndex v 9
    a44 = unsafePartial $ A.unsafeIndex v 12
    a55 = unsafePartial $ A.unsafeIndex v 14
    a = [a11,a22,a33,a44,a55]


tr :: Jac53 -> Jac35
tr (Jac {v}) = Jac {v:v'} where
  a11 = unsafePartial $ A.unsafeIndex v 0
  a12 = unsafePartial $ A.unsafeIndex v 1
  a13 = unsafePartial $ A.unsafeIndex v 2
  a14 = unsafePartial $ A.unsafeIndex v 3
  a15 = unsafePartial $ A.unsafeIndex v 4
  a21 = unsafePartial $ A.unsafeIndex v 5
  a22 = unsafePartial $ A.unsafeIndex v 6
  a23 = unsafePartial $ A.unsafeIndex v 7
  a24 = unsafePartial $ A.unsafeIndex v 8
  a25 = unsafePartial $ A.unsafeIndex v 9
  a31 = unsafePartial $ A.unsafeIndex v 10
  a32 = unsafePartial $ A.unsafeIndex v 11
  a33 = unsafePartial $ A.unsafeIndex v 12
  a34 = unsafePartial $ A.unsafeIndex v 13
  a35 = unsafePartial $ A.unsafeIndex v 14
  v' = [a11,a21,a31,a12,a22,a32,a13,a23,a33,a14,a24,a34,a15,a25,a35]

instance semiringCov3 :: Semiring (Cov Dim3) where
  add (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (+) v1 v2}
  zero = Cov {v: A.replicate 6 0.0 }
  mul (Cov {v: a}) (Cov {v: b}) = Cov {v: c} where
    a11 = unsafePartial $ A.unsafeIndex a 0
    a12 = unsafePartial $ A.unsafeIndex a 1
    a13 = unsafePartial $ A.unsafeIndex a 2
    a22 = unsafePartial $ A.unsafeIndex a 3
    a23 = unsafePartial $ A.unsafeIndex a 4
    a33 = unsafePartial $ A.unsafeIndex a 5
    b11 = unsafePartial $ A.unsafeIndex b 0
    b12 = unsafePartial $ A.unsafeIndex b 1
    b13 = unsafePartial $ A.unsafeIndex b 2
    b22 = unsafePartial $ A.unsafeIndex b 3
    b23 = unsafePartial $ A.unsafeIndex b 4
    b33 = unsafePartial $ A.unsafeIndex b 5
    c = [ a11*b11 + a12*b12 + a13*b13
        , a11*b12 + a12*b22 + a13*b23
        , a11*b13 + a12*b23 + a13*b33
    --  , a12*b11 + a22*b12 + a23*b13
        , a12*b12 + a22*b22 + a23*b23
        , a12*b13 + a22*b23 + a23*b33
    --  , a13*b11 + a23*b12 + a33*b13
    --  , a13*b12 + a23*b22 + a33*b23
        , a13*b13 + a23*b23 + a33*b33
        ]
  one = Cov { v: [1.0, 0.0, 0.0, 1.0, 0.0, 1.0] }
instance ringCov3 :: Ring (Cov Dim3) where
  sub (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (-) v1 v2}
instance showCov3 :: Show (Cov Dim3) where
  show c = "Show (Cov Dim3) \n" <> (show $ toMatrix c)

instance semiringCov4 :: Semiring (Cov Dim4) where
  add (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (+) v1 v2}
  zero = Cov {v: A.replicate 10 0.0 }
  mul a b = c where
    ma = toMatrix a
    mb = toMatrix b
    mc = ma * mb
    c = fromArray $ M.toArray mc
  mul (Cov {v: a}) (Cov {v: b}) = Cov {v: c} where
    a11 = unsafePartial $ A.unsafeIndex a 0
    a12 = unsafePartial $ A.unsafeIndex a 1
    a13 = unsafePartial $ A.unsafeIndex a 2
    a14 = unsafePartial $ A.unsafeIndex a 3
    a22 = unsafePartial $ A.unsafeIndex a 4
    a23 = unsafePartial $ A.unsafeIndex a 5
    a24 = unsafePartial $ A.unsafeIndex a 6
    a33 = unsafePartial $ A.unsafeIndex a 7
    a34 = unsafePartial $ A.unsafeIndex a 8
    a44 = unsafePartial $ A.unsafeIndex a 9
    b11 = unsafePartial $ A.unsafeIndex b 0
    b12 = unsafePartial $ A.unsafeIndex b 1
    b13 = unsafePartial $ A.unsafeIndex b 2
    b14 = unsafePartial $ A.unsafeIndex b 3
    b22 = unsafePartial $ A.unsafeIndex b 4
    b23 = unsafePartial $ A.unsafeIndex b 5
    b24 = unsafePartial $ A.unsafeIndex b 6
    b33 = unsafePartial $ A.unsafeIndex b 7
    b34 = unsafePartial $ A.unsafeIndex b 8
    b44 = unsafePartial $ A.unsafeIndex b 9
    c = [ a11*b11 + a12*b12 + a13*b13 + a14*b14
        , a11*b12 + a12*b22 + a13*b23 + a14*b24
        , a11*b13 + a12*b23 + a13*b33 + a14*b34
        , a11*b14 + a12*b24 + a13*b34 + a14*b44
   --   , a12*b11 + a22*b12 + a23*b13 + a24*b14
        , a12*b12 + a22*b22 + a23*b23 + a24*b24
        , a12*b13 + a22*b23 + a23*b33 + a24*b34
        , a12*b14 + a22*b24 + a23*b34 + a24*b44
   --   , a13*b11 + a23*b12 + a33*b13 + a34*b14
   --   , a13*b12 + a23*b22 + a33*b23 + a34*b24
        , a13*b13 + a23*b23 + a33*b33 + a34*b34
        , a13*b14 + a23*b24 + a33*b34 + a34*b44
   --   , a14*b11 + a24*b12 + a34*b13 + a44*b14
   --   , a14*b12 + a24*b22 + a34*b23 + a44*b24
   --   , a14*b13 + a24*b23 + a34*b33 + a44*b34
        , a14*b14 + a24*b24 + a34*b34 + a44*b44
        ]
  one = Cov { v: [1.0,0.0,0.0,0.0,1.0,0.0,0.0,1.0,0.0,1.0] }
instance ringCov4 :: Ring (Cov Dim4) where
  sub (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (-) v1 v2}
instance showCov4 :: Show (Cov Dim4) where
  show c = "Show (Cov Dim4) \n" <> (show $ toMatrix c)

instance semiringCov5 :: Semiring (Cov Dim5) where
  add (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (+) v1 v2}
  zero = Cov {v: A.replicate 15 0.0 }
  mul (Cov {v: a}) (Cov {v: b}) = Cov {v: c} where
    a11 = unsafePartial $ A.unsafeIndex a 0
    a12 = unsafePartial $ A.unsafeIndex a 1
    a13 = unsafePartial $ A.unsafeIndex a 2
    a14 = unsafePartial $ A.unsafeIndex a 3
    a15 = unsafePartial $ A.unsafeIndex a 4
    a22 = unsafePartial $ A.unsafeIndex a 5
    a23 = unsafePartial $ A.unsafeIndex a 6
    a24 = unsafePartial $ A.unsafeIndex a 7
    a25 = unsafePartial $ A.unsafeIndex a 8
    a33 = unsafePartial $ A.unsafeIndex a 9
    a34 = unsafePartial $ A.unsafeIndex a 10
    a35 = unsafePartial $ A.unsafeIndex a 11
    a44 = unsafePartial $ A.unsafeIndex a 12
    a45 = unsafePartial $ A.unsafeIndex a 13
    a55 = unsafePartial $ A.unsafeIndex a 14
    b11 = unsafePartial $ A.unsafeIndex b 0
    b12 = unsafePartial $ A.unsafeIndex b 1
    b13 = unsafePartial $ A.unsafeIndex b 2
    b14 = unsafePartial $ A.unsafeIndex b 3
    b15 = unsafePartial $ A.unsafeIndex b 4
    b22 = unsafePartial $ A.unsafeIndex b 5
    b23 = unsafePartial $ A.unsafeIndex b 6
    b24 = unsafePartial $ A.unsafeIndex b 7
    b25 = unsafePartial $ A.unsafeIndex b 8
    b33 = unsafePartial $ A.unsafeIndex b 9
    b34 = unsafePartial $ A.unsafeIndex b 10
    b35 = unsafePartial $ A.unsafeIndex b 11
    b44 = unsafePartial $ A.unsafeIndex b 12
    b45 = unsafePartial $ A.unsafeIndex b 13
    b55 = unsafePartial $ A.unsafeIndex b 14
    c = [ a11*b11 + a12*b12 + a13*b13 + a14*b14 + a15*b15
        , a11*b12 + a12*b22 + a13*b23 + a14*b24 + a15*b25
        , a11*b13 + a12*b23 + a13*b33 + a14*b34 + a15*b35
        , a11*b14 + a12*b24 + a13*b34 + a14*b44 + a15*b45
        , a11*b15 + a12*b25 + a13*b35 + a14*b45 + a15*b55
   --   , a12*b11 + a22*b12 + a23*b13 + a24*b14 + a25*b15
        , a12*b12 + a22*b22 + a23*b23 + a24*b24 + a25*b25
        , a12*b13 + a22*b23 + a23*b33 + a24*b34 + a25*b35
        , a12*b14 + a22*b24 + a23*b34 + a24*b44 + a25*b45
        , a12*b15 + a22*b25 + a23*b35 + a24*b45 + a25*b55
   --   , a13*b11 + a23*b12 + a33*b13 + a34*b14 + a35*b15
   --   , a13*b12 + a23*b22 + a33*b23 + a34*b24 + a35*b25
        , a13*b13 + a23*b23 + a33*b33 + a34*b34 + a35*b35
        , a13*b14 + a23*b24 + a33*b34 + a34*b44 + a35*b45
        , a13*b15 + a23*b25 + a33*b35 + a34*b45 + a35*b55
   --   , a14*b11 + a24*b12 + a34*b13 + a44*b14 + a45*b15
   --   , a14*b12 + a24*b22 + a34*b23 + a44*b24 + a45*b25
   --   , a14*b13 + a24*b23 + a34*b33 + a44*b34 + a45*b35
        , a14*b14 + a24*b24 + a34*b34 + a44*b44 + a45*b45
        , a14*b15 + a24*b25 + a34*b35 + a44*b45 + a45*b55
   --   , a15*b11 + a25*b21 + a35*b13 + a45*b14 + a55*b15
   --   , a15*b12 + a25*b22 + a35*b23 + a45*b24 + a55*b25
   --   , a15*b13 + a25*b23 + a35*b33 + a45*b34 + a55*b35
   --   , a15*b14 + a25*b23 + a35*b34 + a45*b44 + a55*b45
        , a15*b15 + a25*b25 + a35*b35 + a45*b45 + a55*b55
        ]
  one = Cov { v: [1.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0,0.0,0.0,1.0,0.0,1.0] }
instance ringCov5 :: Ring (Cov Dim5) where
  sub (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (-) v1 v2}
instance showCov5 :: Show (Cov Dim5) where
  show c = "Show Cov\n" <> (show $ toMatrix c)

instance semiringJac :: Semiring (Jac a b) where
  add (Jac {v: v1}) (Jac {v: v2}) = Jac {v: A.zipWith (+) v1 v2}
  zero = undefined -- Jac {v: A.replicate 15 0.0 }
  mul (Jac {v: v1}) (Jac {v: v2}) = undefined -- Cov {v: cov5StdMult v1 v2}
  one = undefined -- Cov { v: [1.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0,0.0,0.0,1.0,0.0,1.0] }
instance ringJac :: Ring (Jac a b) where
  sub (Jac {v: v1}) (Jac {v: v2}) = Jac {v: A.zipWith (-) v1 v2}
instance showJac :: Show (Jac a b) where
  show (Jac {v}) = "Show Jac \n" <> (show $ v)

-- Instances for Vec -- these are always column vectors
instance semiringVec3 :: Semiring (Vec Dim3) where
add (Vec {v: v1}) (Vec {v: v2}) = Vec {v: A.zipWith (+) v1 v2}
zero = Vec {v: A.replicate 3 0.0 }
mul (Vec {v: v1}) (Vec {v: v2}) = undefined --Vec {v: A.singleton $ A.foldl (+) zero $ A.zipWith (*) v1 v2}
one = Vec { v: A.replicate 3 1.0 }
instance ringVec3 :: Ring (Vec Dim3) where
sub (Vec {v: v1}) (Vec {v: v2}) = Vec {v: A.zipWith (-) v1 v2}
instance showVec3 :: Show (Vec Dim3) where
  show (Vec {v}) = "Show Vec\n" <> (show $ M.fromArray (A.length v) v)

instance semiringVec4 :: Semiring (Vec Dim4) where
add (Vec {v: v1}) (Vec {v: v2}) = Vec {v: A.zipWith (+) v1 v2}
zero = Vec {v: A.replicate 4 0.0 }
mul (Vec {v: v1}) (Vec {v: v2}) = undefined
one = Vec { v: A.replicate 4 1.0 }
instance ringVec4 :: Ring (Vec Dim4) where
sub (Vec {v: v1}) (Vec {v: v2}) = Vec {v: A.zipWith (-) v1 v2}
instance showVec4 :: Show (Vec Dim4) where
  show (Vec {v}) = "Show Vec\n" <> (show $ M.fromArray (A.length v) v)

instance semiringVec5 :: Semiring (Vec Dim5) where
add (Vec {v: v1}) (Vec {v: v2}) = Vec {v: A.zipWith (+) v1 v2}
zero = Vec {v: A.replicate 5 0.0 }
mul (Vec {v: v1}) (Vec {v: v2}) = undefined
one = Vec { v: A.replicate 5 1.0 }
instance ringVec5 :: Ring (Vec Dim5) where
sub (Vec {v: v1}) (Vec {v: v2}) = Vec {v: A.zipWith (-) v1 v2}
instance showVec5 :: Show (Vec Dim5) where
  show (Vec {v}) = "Show Vec\n" <> (show $ M.fromArray (A.length v) v)


class Chi2 a where
  chi2 :: Vec a -> Cov a -> Number
  sandwichvv :: Vec a -> Vec a -> Number
infix 7 chi2 as |*|
infix 7 sandwichvv as |.|

instance chi2Dim5 :: Chi2 Dim5 where
  chi2 v c = x where
    mv = toMatrix v
    mc = toMatrix c
    mx = M.tr mv * mc * mv
    x = M.scalar mx
  sandwichvv (Vec {v:v1}) (Vec {v:v2}) = vv where
    vv = A.foldl (+) zero $ A.zipWith (*) v1 v2
instance chi2Dim3 :: Chi2 Dim3 where
  chi2 v c = x where
    mv = toMatrix v
    mc = toMatrix c
    mx = M.tr mv * mc * mv
    x = M.scalar mx
  sandwichvv (Vec {v:v1}) (Vec {v:v2}) = vv where
    vv = A.foldl (+) zero $ A.zipWith (*) v1 v2

class TMat a b where
  sandwich :: Jac a b -> Cov a -> Cov b
  transcov :: Jac a b -> Cov b -> Jac a b
  transvoc :: Cov a -> Jac a b -> Jac a b
  transvec :: Jac a b -> Vec b -> Vec a
  sandwichjj :: Jac a b -> Jac b a -> Cov a
infixr 7 sandwich as ||*||
infixr 7 transcov as ||*
infixr 7 transvoc as *||
infixr 7 transvec as |||
infixr 7 sandwichjj as ||||

instance tmat34 :: TMat Dim3 Dim4 where
  sandwich j c = c' where
    mj = toMatrix j
    mc = toMatrix c
    mc' = M.tr mj * mc * mj
    c' = fromArray $ M.toArray mc'
  transcov j c = j' where
    mj = toMatrix j
    mc = toMatrix c
    mj' = mj * mc
    j' = fromArray $ M.toArray mj'
  transvoc c j = j' where
    mj = toMatrix j
    mc = toMatrix c
    mj' = mc * mj
    j' = fromArray $ M.toArray mj'
  transvec j v = v' where
    mj = toMatrix j
    mv = toMatrix v
    mv' = mj * mv
    v' = fromArray $ M.toArray mv'
  sandwichjj j1 j2 = c' where
    mj1 = toMatrix j1
    mj2 = toMatrix j2
    mc' = mj1 * mj2
    c' = fromArray $ M.toArray mc'
instance tmat35 :: TMat Dim3 Dim5 where
  sandwich j c = c' where
    mj = toMatrix j
    mc = toMatrix c
    mc' = M.tr mj * mc * mj
    c' = fromArray $ M.toArray mc'
  transcov j c = j' where
    mj = toMatrix j
    mc = toMatrix c
    mj' = mj * mc
    j' = fromArray $ M.toArray mj'
  transvoc c j = j' where
    mj = toMatrix j
    mc = toMatrix c
    mj' = mc * mj
    j' = fromArray $ M.toArray mj'
  transvec j v = v' where
    mj = toMatrix j
    mv = toMatrix v
    mv' = mj * mv
    v' = fromArray $ M.toArray mv'
  sandwichjj j1 j2 = c' where
    mj1 = toMatrix j1
    mj2 = toMatrix j2
    mc' = mj1 * mj2
    c' = fromArray $ M.toArray mc'
instance tmat43 :: TMat Dim4 Dim3 where
  sandwich j c = c' where
    mj = toMatrix j
    mc = toMatrix c
    mc' = M.tr mj * mc * mj
    c' = fromArray $ M.toArray mc'
  transcov j c = j' where
    mj = toMatrix j
    mc = toMatrix c
    mj' = mj * mc
    j' = fromArray $ M.toArray mj'
  transvoc c j = j' where
    mj = toMatrix j
    mc = toMatrix c
    mj' = mc * mj
    j' = fromArray $ M.toArray mj'
  transvec j v = v' where
    mj = toMatrix j
    mv = toMatrix v
    mv' = mj * mv
    v' = fromArray $ M.toArray mv'
  sandwichjj j1 j2 = c' where
    mj1 = toMatrix j1
    mj2 = toMatrix j2
    mc' = mj1 * mj2
    c' = fromArray $ M.toArray mc'
instance tmat53 :: TMat Dim5 Dim3 where
  sandwich j c = c' where
    mj = toMatrix j
    mc = toMatrix c
    mc' = M.tr mj * mc * mj
    c' = fromArray $ M.toArray mc'
  transcov j c = j' where
    mj = toMatrix j
    mc = toMatrix c
    mj' = mj * mc
    j' = fromArray $ M.toArray mj'
  transvoc c j = j' where
    mj = toMatrix j
    mc = toMatrix c
    mj' = mc * mj
    j' = fromArray $ M.toArray mj'
  transvec j v = v' where
    mj = toMatrix j
    mv = toMatrix v
    mv' = mj * mv
    v' = fromArray $ M.toArray mv'
  sandwichjj j1 j2 = c' where
    mj1 = toMatrix j1
    mj2 = toMatrix j2
    mc' = mj1 * mj2
    c' = fromArray $ M.toArray mc'
instance tmat55 :: TMat Dim5 Dim5 where
  sandwich j c = c' where
    mj = toMatrix j
    mc = toMatrix c
    mc' = M.tr mj * mc * mj
    c' = fromArray $ M.toArray mc'
  transcov j c = j' where
    mj = toMatrix j
    mc = toMatrix c
    mj' = mj * mc
    j' = fromArray $ M.toArray mj'
  transvoc c j = j' where
    mj = toMatrix j
    mc = toMatrix c
    mj' = mc * mj
    j' = fromArray $ M.toArray mj'
  transvec j v = v' where
    mj = toMatrix j
    mv = toMatrix v
    mv' = mj * mv
    v' = fromArray $ M.toArray mv'
  sandwichjj j1 j2 = c' where
    mj1 = toMatrix j1
    mj2 = toMatrix j2
    mc' = mj1 * mj2
    c' = fromArray $ M.toArray mc'

class TVec a where
  ttransvec :: Cov a -> Vec a -> Vec a
infixr 7 ttransvec as *|
instance tvec3 :: TVec Dim3 where
  ttransvec c v = v' where
    mc = toMatrix c
    mv = toMatrix v
    mv' = mc * mv
    v' = fromArray $ M.toArray mv'
instance tvec5 :: TVec Dim5 where
  ttransvec c v = v' where
    mc = toMatrix c
    mv = toMatrix v
    mv' = mc * mv
    v' = fromArray $ M.toArray mv'

scaleDiag :: Number -> Cov3 -> Cov3
scaleDiag s c = c' where
  mc = toMatrix c
  mc' = M.scaleDiag s mc
  c' = fromArray $ M.toArray mc'

subm :: Int -> Vec5 -> Vec3
subm n (Vec {v:v5}) = Vec {v: v'} where
  a1 = unsafePartial $ A.unsafeIndex v5 0
  a2 = unsafePartial $ A.unsafeIndex v5 1
  a3 = unsafePartial $ A.unsafeIndex v5 2
  v' = [a1,a2,a3]

subm2 :: Int -> Cov5 -> Cov3
subm2 n (Cov {v:v}) = Cov {v: v'} where
  a11 = unsafePartial $ A.unsafeIndex v 0
  a12 = unsafePartial $ A.unsafeIndex v 1
  a13 = unsafePartial $ A.unsafeIndex v 2
  a22 = unsafePartial $ A.unsafeIndex v 5
  a23 = unsafePartial $ A.unsafeIndex v 6
  a33 = unsafePartial $ A.unsafeIndex v 9
  v' = [a11,a12,a13,a22,a23,a33]


newtype MD = MakeMD {m3 :: Cov3, m5 :: Cov5}
instance showMD :: Show MD where
  show (MakeMD {m3, m5}) = "Show MD,\nm3=" <> show m3 <> "\nm5=" <> show m5

testCov = "testCov: " <> show md <> "\n"
                      <> show mm3
                      <> show mm5
                      <> "exp v3 " <> show ( (v3 + v3) |.| v3 ) <> "\n"
                      <> "tj3 " <> show tj3 <> "vv3 " <> show vv3
                      <> show (v3 |*| c3)
                      <> show (c3 * (inv c3))
                      <> show (c4 * (inv c4))
                      <> show c5 <> show (det c5) <> show (inv c5) 
                      <> show (c5 * (inv c5)) 
                      where
  c3 :: Cov3
  c3 = fromArray [1.0,2.0,3.0,4.0,5.0,6.0]
  c4 :: Cov4
  c4 = fromArray [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0]
  c5 :: Cov5
  c5 = fromArray [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0]
  c51 :: Cov5
  c51 = one
  v3 :: Vec3
  v3 = fromArray [10.0,11.0,12.0]
  v5 :: Vec5
  v5 = fromArray [10.0,11.0,12.0,13.0,14.0]
  j53 :: Jac53
  j53 = fromArray [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0]
  tj3 :: Cov3
  tj3 = j53 ||*|| c5
  vv5 :: Vec5
  vv5 = j53 ||| v3
  vv3 :: Vec3
  vv3 = tr j53 ||| j53 ||| c3 *| v3

  m3 :: M.Matrix Number
  m3 = M.fromArray2 3 3 [1.0,2.0,3.0,2.0,4.0,5.0,3.0,5.0,6.0]
  mm3 = (m3+m3)*m3
  m5 :: M.Matrix Number
  m5 = M.fromArray2 5 5 [1.0,2.0,3.0,4.0,5.0, 2.0,6.0,7.0,8.0,9.0
                        ,3.0,7.0,10.0,11.0,12.0, 4.0,8.0,11.0,13.0,14.0
                        ,5.0,9.0,12.0,14.0,15.0]
  mm5 = (m5+m5)*m5
  md = MakeMD {m3: (c3+c3)*c3, m5: (c5+c5)*c5}

{-- det3 :: Cov3 -> Number --}
{-- det3 cov = det where --}
{--   det = --} 

-----------------------------------------------

