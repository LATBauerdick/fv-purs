module Data.Cov
    where

import Prelude
import Data.Array
  ( replicate, unsafeIndex, zipWith, length, foldl, range, take
  ) as A
import Control.Monad.Eff ( forE )
import Control.Monad.ST ( pureST )
import Data.Array.ST (emptySTArray, peekSTArray, pokeSTArray
                     , pushAllSTArray, unsafeFreeze)
import Data.Foldable ( sum )
import Partial.Unsafe ( unsafePartial )
import Data.Maybe ( Maybe (..) )
import Control.MonadZero (guard)
import Data.Int ( toNumber, ceil )
import Math ( abs, sqrt )
import Unsafe.Coerce ( unsafeCoerce ) as Unsafe.Coerce

import Data.SimpleMatrix
  ( Matrix
  , transpose
  , fromArray, fromArray2, toArray
  ) as M
import Stuff

newtype Dim3 = DDim3 Int
newtype Dim4 = DDim4 Int
newtype Dim5 = DDim5 Int
class DDim a where
  ddim :: a -> Int
instance ddim3 :: DDim Dim3 where
  ddim _ = 3
instance ddim4 :: DDim Dim4 where
  ddim _ = 4
instance ddim5 :: DDim Dim5 where
  ddim _ = 5
instance ddima :: DDim a where
  ddim _ = undefined

mapTo :: forall a. DDim a => Cov a -> Dim5
mapTo = Unsafe.Coerce.unsafeCoerce
class Dim a where
  dim :: a -> Int
instance dima :: Dim (Cov a) where
  dim ccc = n where
    xx = mapTo ccc
    n = ddim xx
instance dim3 :: Dim (Cov Dim3) where
  dim ccc = 3
instance dim4 :: Dim (Cov Dim4) where
  dim ccc = 4
instance dim5 :: Dim (Cov Dim5) where
  dim ccc = 5

newtype Cov a = Cov { v :: Array Number }
newtype Jac a b = Jac { v :: Array Number }
newtype Vec a = Vec { v :: Array Number }
type Cov3 = Cov Dim3
type Cov4 = Cov Dim4
type Cov5 = Cov Dim5
type Jac53 = Jac Dim5 Dim3
type Jac43 = Jac Dim4 Dim3
type Jac34 = Jac Dim3 Dim4
{-- type Jac35 = Jac Dim3 Dim5 --}
type Jac33 = Jac Dim3 Dim3
type Jac44 = Jac Dim4 Dim4
type Jac55 = Jac Dim5 Dim5
type Vec3 = Vec Dim3
type Vec4 = Vec Dim4
type Vec5 = Vec Dim5
type Jacs = {aa :: Jac53, bb :: Jac53, h0 :: Vec5}

-- access to arrays of symmetrical matrices
uGet :: Array Number -> Int -> Int -> Int -> Number
uGet a w i j | i <= j = unsafePartial $ A.unsafeIndex a
                                        ((i-1)*w - (i-1)*(i-2)/2 + j-i)
             | otherwise = unsafePartial $ A.unsafeIndex a
                                        ((j-1)*w - (j-1)*(j-2)/2 + i-j)
-------------------------------------------------------------------------
-------------------------------------------------------------------------
-------------------------------------------------------------------------
-------------------------------------------------------------------------
-- Mat to give behavior to Cov and Vec and Jac
-- ability to convert to and from Matrix and Array
-- while keeping info about dimensionality
-- also define Semiring and Ring functions
--
-------------------------------------------------------------------------
-------------------------------------------------------------------------
-------------------------------------------------------------------------

class AdderFunDeps a b c | a b -> c where
    myPlusFunDeps :: a -> b -> c

instance addIntInt :: AdderFunDeps Int Int Int where
    myPlusFunDeps a b = a + b

instance addNumNum :: AdderFunDeps Int Number Number where
    myPlusFunDeps a b = toNumber a + b

testFunDeps :: String
-- This works because of the fundeps.
testFunDeps = show $ myPlusFunDeps (3 :: Int) (4 :: Int)

class Matt a where
  val :: a -> Array Number
  fromArray :: Array Number -> a
  toArray :: a -> Array Number
instance matCova :: Matt (Cov a) where
  val (Cov {v}) = v
  fromArray a = c' where
    l = A.length a
    c' = case l of
              6   -> Cov {v: a}
              10  -> Cov {v: a}
              15  -> Cov {v: a}
              _   -> Cov {v: do -- only upper triangle
                            let n =ceil (sqrt (toNumber l))
                            i0 <- A.range 0 (n-1)
                            j0 <- A.range i0 (n-1)
                            pure $ uidx a (i0*n+j0) }
  toArray c@(Cov {v}) = v' where
          l = A.length v
          n = ceil ((sqrt(8.0 * (toNumber l) + 1.0) - 1.0)/2.0)
          idx :: Int -> Int -> Int -- index into values array of symmetric matrices
          idx i0 j0 | i0 <= j0    = (i0*n - i0*(i0-1)/2 + j0-i0)
                    | otherwise = (j0*n - j0*(j0-1)/2 + i0-j0)
          v' = do
                  i0 <- A.range 0 (n-1)
                  j0 <- A.range 0 (n-1)
                  pure $ uidx v (idx i0 j0)
instance matVeca :: Matt (Vec a) where
  val (Vec {v}) = v
  fromArray a = Vec {v: a}
  toArray (Vec {v}) = v
instance matJacab :: Matt (Jac a b) where
  val (Jac {v}) = v
  fromArray a = Jac {v: a}
  toArray (Jac {v}) = v

class Matt1 a where
  toMatrix :: a -> M.Matrix
instance mat1Cova :: Matt1 (Cov a) where
  toMatrix a@(Cov {v}) = case A.length v of
                            6  -> M.fromArray2 3 3 v
                            10 -> M.fromArray2 4 4 v
                            15 -> M.fromArray2 5 5 v
                            _ -> error $ "--------------mat1Cova toMatrix "
                                          <> show (A.length v)
instance mat1Veca :: Matt1 (Vec a) where
  toMatrix (Vec {v}) = M.fromArray (A.length v) v
instance mat1Jacaa :: Matt1 (Jac a b) where
  toMatrix j@(Jac {v}) = case A.length v of
                              9  -> M.fromArray2 3 3 v
                              16 -> M.fromArray2 4 4 v
                              25 -> M.fromArray2 5 5 v
                              12 -> M.fromArray2 4 3 v `debug` "this should not have happened ??????????????????? 4 3"
                              15 -> M.fromArray2 5 3 v `debug` "this should not have happened ??????????????????? 4 3"
                              _  -> error $ "----------------mat1Jacaa toMatrix "
                                          <> show (A.length v)
instance mat1Jac43 :: Matt1 (Jac Dim4 Dim3) where
  toMatrix (Jac {v}) = M.fromArray2 4 3 v `debug` "WTF??? 4 3"
instance mat1Jac34 :: Matt1 (Jac Dim3 Dim4) where
  toMatrix (Jac {v}) = M.fromArray2 3 4 v `debug` "WTF??? 3 4"
instance mat1Jac53 :: Matt1 (Jac Dim5 Dim3) where
  toMatrix (Jac {v}) = M.fromArray2 5 3 v `debug` "WTF??? 5 3"
instance mat1Jac35 :: Matt1 (Jac Dim3 Dim5) where
  toMatrix (Jac {v}) = M.fromArray2 3 5 v `debug` "WTF??? 3 5"
{-- instance mat1Jacab :: Matt1 (Jac a b) where --}
{--   toMatrix (Jac {v}) =  error $ "xxxxxxxxxxxxxxxxxxxxtoMatrix Jac a b " --}
{--                                 <> show (A.length v) --}
--{{{
{-- instance matCov3 :: Matt (Cov Dim3) where --}
{--   val (Cov {v}) = v --}
{--   fromArray a | A.length a == 6 = Cov {v: a} --}
{--               | A.length a == 9 = Cov {v: a'} where --}
{--     a11 = unsafePartial $ A.unsafeIndex a 0 --}
{--     a12 = unsafePartial $ A.unsafeIndex a 1 --}
{--     a13 = unsafePartial $ A.unsafeIndex a 2 --}
{--     a22 = unsafePartial $ A.unsafeIndex a 4 --}
{--     a23 = unsafePartial $ A.unsafeIndex a 5 --}
{--     a33 = unsafePartial $ A.unsafeIndex a 8 --}
{--     a' = [a11,a12,a13,a22,a23,a33] --}
{--               | otherwise = error $ "Cov3 fromArray: wrong input array length " --}
{--                                   <> show (A.length a) --}
{--   toArray (Cov {v}) = vv where --}
{--     a11 = unsafePartial $ A.unsafeIndex v 0 --}
{--     a12 = unsafePartial $ A.unsafeIndex v 1 --}
{--     a13 = unsafePartial $ A.unsafeIndex v 2 --}
{--     a22 = unsafePartial $ A.unsafeIndex v 3 --}
{--     a23 = unsafePartial $ A.unsafeIndex v 4 --}
{--     a33 = unsafePartial $ A.unsafeIndex v 5 --}
{--     vv = [a11, a12, a13, a12, a22, a23, a13, a23, a33] --}
{--   toMatrix c = M.fromArray2 3 3 $ toArray c --}
{-- instance matCov4 :: Matt (Cov Dim4) where --}
{--   val (Cov {v}) = v --}
{--   fromArray a | A.length a == 10 = Cov {v: a} --}
{--               | A.length a == 16 = Cov {v: a'} where --}
{--     a11 = unsafePartial $ A.unsafeIndex a 0 --}
{--     a12 = unsafePartial $ A.unsafeIndex a 1 --}
{--     a13 = unsafePartial $ A.unsafeIndex a 2 --}
{--     a14 = unsafePartial $ A.unsafeIndex a 3 --}
{--     a22 = unsafePartial $ A.unsafeIndex a 5 --}
{--     a23 = unsafePartial $ A.unsafeIndex a 6 --}
{--     a24 = unsafePartial $ A.unsafeIndex a 7 --}
{--     a33 = unsafePartial $ A.unsafeIndex a 10 --}
{--     a34 = unsafePartial $ A.unsafeIndex a 11 --}
{--     a44 = unsafePartial $ A.unsafeIndex a 15 --}
{--     a' = [a11,a12,a13,a14,a22,a23,a24,a33,a34,a44] --}
{--               | otherwise = error $ "Cov4 fromArray: wrong input array length " --}
{--                                   <> show (A.length a) --}
{--   toArray (Cov {v}) = vv where --}
{--     a11 = unsafePartial $ A.unsafeIndex v 0 --}
{--     a12 = unsafePartial $ A.unsafeIndex v 1 --}
{--     a13 = unsafePartial $ A.unsafeIndex v 2 --}
{--     a14 = unsafePartial $ A.unsafeIndex v 3 --}
{--     a22 = unsafePartial $ A.unsafeIndex v 4 --}
{--     a23 = unsafePartial $ A.unsafeIndex v 5 --}
{--     a24 = unsafePartial $ A.unsafeIndex v 6 --}
{--     a33 = unsafePartial $ A.unsafeIndex v 7 --}
{--     a34 = unsafePartial $ A.unsafeIndex v 8 --}
{--     a44 = unsafePartial $ A.unsafeIndex v 9 --}
{--     vv = [a11,a12,a13,a14,a12,a22,a23,a24,a13,a23,a33,a34,a14,a24,a34,a44] --}
{--   toMatrix c = M.fromArray2 4 4 $ toArray c --}
{-- instance matCov5 :: Matt (Cov Dim5) where --}
{--   val (Cov {v}) = v --}
{--   fromArray a | A.length a == 15 = Cov {v: a} --}
{--               | A.length a == 25 = Cov {v: a'} where --}
{--     a11 = unsafePartial $ A.unsafeIndex a 0 --}
{--     a12 = unsafePartial $ A.unsafeIndex a 1 --}
{--     a13 = unsafePartial $ A.unsafeIndex a 2 --}
{--     a14 = unsafePartial $ A.unsafeIndex a 3 --}
{--     a15 = unsafePartial $ A.unsafeIndex a 4 --}
{--     a22 = unsafePartial $ A.unsafeIndex a 6 --}
{--     a23 = unsafePartial $ A.unsafeIndex a 7 --}
{--     a24 = unsafePartial $ A.unsafeIndex a 8 --}
{--     a25 = unsafePartial $ A.unsafeIndex a 9 --}
{--     a33 = unsafePartial $ A.unsafeIndex a 12 --}
{--     a34 = unsafePartial $ A.unsafeIndex a 13 --}
{--     a35 = unsafePartial $ A.unsafeIndex a 14 --}
{--     a44 = unsafePartial $ A.unsafeIndex a 18 --}
{--     a45 = unsafePartial $ A.unsafeIndex a 19 --}
{--     a55 = unsafePartial $ A.unsafeIndex a 24 --}
{--     a' = [a11,a12,a13,a14,a15,a22,a23,a24,a25,a33,a34,a35,a44,a45,a55] --}
{--               | otherwise = error "Cov5 fromArray:  wrong input array length" --}
{--   toArray (Cov {v}) = vv where --}
{--     a11 = unsafePartial $ A.unsafeIndex v 0 --}
{--     a12 = unsafePartial $ A.unsafeIndex v 1 --}
{--     a13 = unsafePartial $ A.unsafeIndex v 2 --}
{--     a14 = unsafePartial $ A.unsafeIndex v 3 --}
{--     a15 = unsafePartial $ A.unsafeIndex v 4 --}
{--     a22 = unsafePartial $ A.unsafeIndex v 5 --}
{--     a23 = unsafePartial $ A.unsafeIndex v 6 --}
{--     a24 = unsafePartial $ A.unsafeIndex v 7 --}
{--     a25 = unsafePartial $ A.unsafeIndex v 8 --}
{--     a33 = unsafePartial $ A.unsafeIndex v 9 --}
{--     a34 = unsafePartial $ A.unsafeIndex v 10 --}
{--     a35 = unsafePartial $ A.unsafeIndex v 11 --}
{--     a44 = unsafePartial $ A.unsafeIndex v 12 --}
{--     a45 = unsafePartial $ A.unsafeIndex v 13 --}
{--     a55 = unsafePartial $ A.unsafeIndex v 14 --}
{--     vv = [a11, a12, a13, a14, a15, a12, a22, a23, a24, a25 --}
{--          ,a13, a23, a33, a34, a35, a14, a24, a34, a44, a45 --}
{--          ,a15, a25, a35, a45, a55] --}
{--   toMatrix c = M.fromArray2 5 5 $ toArray c --}
{-- instance matVec3 :: Matt (Vec Dim3) where --}
{--   val (Vec {v}) = v --}
{--   fromArray a | A.length a /= 3 = --}
{--                   error "Vec3 fromArray: wrong input array length" --}
{--               | otherwise = Vec {v: a} --}
{--   toArray (Vec {v}) = v --}
{--   toMatrix (Vec {v}) = M.fromArray 3 v --}
{-- instance matVec4 :: Matt (Vec Dim4) where --}
{--   val (Vec {v}) = v --}
{--   fromArray a | A.length a /= 4 = --}
{--                   error "Vec4 fromArray: wrong input array length" --}
{--               | otherwise = Vec {v: a} --}
{--   toArray (Vec {v}) = v --}
{--   toMatrix (Vec {v}) = M.fromArray 4 v --}
{-- instance matVec5 :: Matt (Vec Dim5) where --}
{--   val (Vec {v}) = v --}
{--   fromArray a | A.length a /= 5 = --}
{--                   error "Vec5 fromArray: wrong input array length" --}
{--               | otherwise = Vec {v: a} --}
{--   toArray (Vec {v}) = v --}
{--   toMatrix (Vec {v}) = M.fromArray 5 v --}
--}}}
-----------------------------------------------------------------
-- | funcitons for symetric matrices: Cov
-- | type class SymMat
class SymMat a where
  inv :: Cov a -> Cov a                -- | inverse matrix
  invMaybe :: Cov a -> Maybe (Cov a)   -- | Maybe inverse matrix
  det :: Cov a -> Number               -- | determinant
  diag :: Cov a -> Array Number        -- | Array of diagonal elements
  chol :: Cov a -> Jac a a             -- | Cholsky decomposition
instance symMatCov3 :: SymMat Dim3 where
  inv m = uJust (invMaybe m)
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
    pure $ fromArray v'
  chol a = choldc a 3
  det (Cov {v}) = dd where
        a = unsafePartial $ A.unsafeIndex v 0
        b = unsafePartial $ A.unsafeIndex v 1
        c = unsafePartial $ A.unsafeIndex v 2
        d = unsafePartial $ A.unsafeIndex v 3
        e = unsafePartial $ A.unsafeIndex v 4
        f = unsafePartial $ A.unsafeIndex v 5
        dd = a*d*f - a*e*e - b*b*f + 2.0*b*c*e - c*c*d
  diag (Cov {v}) = a where
    a11 = unsafePartial $ A.unsafeIndex v 0
    a22 = unsafePartial $ A.unsafeIndex v 3
    a33 = unsafePartial $ A.unsafeIndex v 5
    a = [a11,a22,a33]
instance symMatCov4 :: SymMat Dim4 where
  inv m = uJust (invMaybe m)
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
    pure $ fromArray v'
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
  chol a = choldc a 4
instance symMatCov5 :: SymMat Dim5 where
  inv m = cholInv m 5
  invMaybe m = Just (cholInv m 5)
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
      + 2.0*b*c*h*l*n + 2.0*b*c*i*k*n - 2.0*b*c*i*l*m - 2.0*b*d*g*k*o
      + 2.0*b*d*g*l*n + 2.0*b*d*h*j*o - 2.0*b*d*h*l*l - 2.0*b*d*i*j*n
      + 2.0*b*d*i*k*l + 2.0*b*e*g*k*n - 2.0*b*e*g*l*m - 2.0*b*e*h*j*n
      + 2.0*b*e*h*k*l + 2.0*b*e*i*j*m - 2.0*b*e*i*k*k - c*c*f*m*o + c*c*f*n*n
      + c*c*h*h*o - 2.0*c*c*h*i*n + c*c*i*i*m + 2.0*c*d*f*k*o - 2.0*c*d*f*l*n
      - 2.0*c*d*g*h*o + 2.0*c*d*g*i*n + 2.0*c*d*h*i*l - 2.0*c*d*i*i*k
      - 2.0*c*e*f*k*n + 2.0*c*e*f*l*m + 2.0*c*e*g*h*n - 2.0*c*e*g*i*m
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
  chol a = choldc a 5

class MulMat a b c | a b -> c where
  mulm :: a -> b -> c
infixr 7 mulm as *.
instance mulMata :: MulMat (Cov a) (Cov a) (Jac a a) where
  mulm c1 c2 = j' where
    mc1 = toMatrix c1
    mc2 = toMatrix c2
    mj' = mc1 * mc2
    j' = fromArray $ M.toArray mj'
instance mulMatJC :: MulMat (Jac a b) (Cov b) (Jac a b) where
  mulm j c = j' where
    mj = toMatrix j
    mc = toMatrix c
    mj' = mj * mc
    j' = fromArray $ M.toArray mj'
instance mulMatCJ :: MulMat (Cov a) (Jac a b) (Jac a b) where
  mulm c j = j' where
    mc = toMatrix c
    mj = toMatrix j
    mj' = mc * mj
    j' = fromArray $ M.toArray mj'
instance mulMatJV :: MulMat (Jac a b) (Vec b) (Vec a) where
  mulm j v = v' where
    mj = toMatrix j
    mv = toMatrix v
    mv' = mj * mv
    v' = fromArray $ M.toArray mv'
instance mulMatJJ :: MulMat (Jac a b) (Jac b a) (Jac a a) where
  mulm j1 j2 = j' where
    mj1 = toMatrix j1
    mj2 = toMatrix j2
    mj' = mj1 * mj2
    j' = fromArray $ M.toArray mj'
instance mulMatCV :: MulMat (Cov a) (Vec a) (Vec a) where
  mulm c v = v' where
    mc = toMatrix c
    mv = toMatrix v
    mv' = mc * mv
    v' = fromArray $ M.toArray mv'
instance mulMatVV :: MulMat (Vec a) (Vec a) Number where
  mulm (Vec {v:v1}) (Vec {v:v2}) = A.foldl (+) zero $ A.zipWith (*) v1 v2
class TrMat a b | a -> b where
  tr :: a -> b
instance trMatC :: TrMat (Cov a) (Cov a) where
  tr c = c
instance trMatJ :: TrMat (Jac a b) (Jac b a) where
  tr j = jt where
    mj = toMatrix j
    mjt = M.transpose mj
    jt = fromArray $ M.toArray mjt
class SW a b c | a b -> c where
  sw :: a -> b -> c
infixl 7 sw as <.>
instance swVec :: SW (Vec a) (Cov a) Number where
  sw v c = n where
    mv = toMatrix v
    mc = toMatrix c
    mc' = M.transpose mv * mc * mv
    n = uidx (M.toArray mc') 0
instance swJac :: SW (Jac a b) (Cov a) (Cov b) where
  sw j c = c' where
    mj = toMatrix j
    mc = toMatrix c
    mc' = M.transpose mj * mc * mj
    c' = fromArray $ M.toArray mc'
testCov2 :: String
testCov2 = s where
  s = "Test Cov 2----------------------------------------------\n"
    <> "Vec *. Vec = " <> show (v3 *. v3) <> "\n"
    <> "Cov *. Cov = " <> show ((one::Cov3) *. inv (one::Cov3)) <> "\n"
    <> "Vec + Vec = " <> show (v5 + v5) <> "\n"
    <> "chol Cov = " <> show (chol (one::Cov5)) <> "\n"
    <> "Vec <.> Cov = " <> show (v5 <.> inv (one::Cov5)) <> "\n"
  v3 = fromArray [1.0,1.0,1.0] :: Vec3
  v5 = fromArray [1.0,1.0,1.0,1.0,1.0] :: Vec5

instance semiringCov3 :: Semiring (Cov Dim3) where
  add (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (+) v1 v2}
  zero = Cov {v: A.replicate 6 0.0 }
  mul (Cov {v: a}) (Cov {v: b}) = error "------------> mul cov3 * cov3 not allowed"
  one = Cov { v: [1.0, 0.0, 0.0, 1.0, 0.0, 1.0] }
instance ringCov3 :: Ring (Cov Dim3) where
  sub (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (-) v1 v2}
instance showCov3 :: Show (Cov Dim3) where
  show c = "Show (Cov Dim3) \n" <> (show $ toMatrix c)

instance semiringCov4 :: Semiring (Cov Dim4) where
  add (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (+) v1 v2}
  zero = Cov {v: A.replicate 10 0.0 }
  mul (Cov {v: a}) (Cov {v: b}) = error "------------> mul cov4 * cov4 not allowed"
  one = Cov { v: [1.0,0.0,0.0,0.0,1.0,0.0,0.0,1.0,0.0,1.0] }
instance ringCov4 :: Ring (Cov Dim4) where
  sub (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (-) v1 v2}
instance showCov4 :: Show (Cov Dim4) where
  show c = "Show (Cov Dim4)\n" <> (show $ toMatrix c)

instance semiringCov5 :: Semiring (Cov Dim5) where
  add (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (+) v1 v2}
  zero = Cov {v: A.replicate 15 0.0 }
  mul a b = error "------------> mul cov5 * cov5 not allowed"
  one = Cov { v: [1.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0,0.0,0.0,1.0,0.0,1.0] }
instance ringCov5 :: Ring (Cov Dim5) where
  sub (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (-) v1 v2}
instance showCov5 :: Show (Cov Dim5) where
  show c = "Show (Cov Dim5)\n" <> (show $ toMatrix c)

instance semiringJac :: Semiring (Jac a b) where
  add (Jac {v: v1}) (Jac {v: v2}) = Jac {v: A.zipWith (+) v1 v2}
  zero = undefined
  mul (Jac {v: v1}) (Jac {v: v2}) = undefined -- Cov {v: cov5StdMult v1 v2}
  one = undefined
instance ringJac :: Ring (Jac a b) where
  sub (Jac {v: v1}) (Jac {v: v2}) = Jac {v: A.zipWith (-) v1 v2}
instance showJac :: Show (Jac a b) where
  show a = "Show Jac\n" <> show (toMatrix a)

-- Instances for Vec -- these are always column vectors
instance semiringVec3 :: Semiring (Vec Dim3) where
  add (Vec {v: v1}) (Vec {v: v2}) = Vec {v: A.zipWith (+) v1 v2}
  zero = Vec {v: A.replicate 3 0.0 }
  mul (Vec {v: v1}) (Vec {v: v2}) = undefined
  one = Vec { v: A.replicate 3 1.0 }

instance semiringVec4 :: Semiring (Vec Dim4) where
  add (Vec {v: v1}) (Vec {v: v2}) = Vec {v: A.zipWith (+) v1 v2}
  zero = Vec {v: A.replicate 4 0.0 }
  mul (Vec {v: v1}) (Vec {v: v2}) = undefined
  one = Vec { v: A.replicate 4 1.0 }

instance semiringVec5 :: Semiring (Vec Dim5) where
  add (Vec {v: v1}) (Vec {v: v2}) = Vec {v: A.zipWith (+) v1 v2}
  zero = Vec {v: A.replicate 5 0.0 }
  mul (Vec {v: v1}) (Vec {v: v2}) = undefined
  one = Vec { v: A.replicate 5 1.0 }

instance semiringVec :: Semiring (Vec a) where
  add (Vec {v: v1}) (Vec {v: v2}) = Vec {v: A.zipWith (+) v1 v2}
  {-- zero = error "error calling zero for Vec a" -- Vec {v: A.replicate 5 0.0 } --}
  zero = Vec {v: A.replicate 5 0.0 } `debug` "xxxxxxxxxxx>>> called Vec zero"
  mul (Vec {v: v1}) (Vec {v: v2}) = undefined
  {-- one = error "error calling one for Vec a" -- Vec { v: A.replicate 5 1.0 } --}
  one = Vec { v: A.replicate 5 1.0 } `debug` "xxxxxxxxxxx>>> called Vec one"
instance ringVec :: Ring (Vec a) where
  sub (Vec {v: v1}) (Vec {v: v2}) = Vec {v: A.zipWith (-) v1 v2}
instance showVec :: Show (Vec a) where
  show v = "Show Vec\n" <> show (toMatrix v)
-------------------------------------------------------------------------
-- Chi2 for Vec * Vec and Vev^T * Cov * Vec, resulting in Number
-- operators |.| and |*|
--
class Chi2 a where
  chi2 :: Vec a -> Cov a -> Number
  sandwichvv :: Vec a -> Vec a -> Number
infix 7 chi2 as |*|
infix 7 sandwichvv as |.|

instance chi2Dim :: Chi2 a where
  chi2 v c = x where
    mv = toMatrix v
    mc = toMatrix c
    mx = M.transpose mv * mc * mv
    x = uidx (M.toArray mx) 0
  sandwichvv (Vec {v:v1}) (Vec {v:v2}) = vv where
    vv = A.foldl (+) zero $ A.zipWith (*) v1 v2
{-- instance chi2Dim5 :: Chi2 Dim5 where --}
{--   chi2 v c = x where --}
{--     mv = toMatrix v --}
{--     mc = toMatrix c --}
{--     mx = M.transpose mv * mc * mv --}
{--     x = uidx (M.toArray mx) 0 --}
{--   sandwichvv (Vec {v:v1}) (Vec {v:v2}) = vv where --}
{--     vv = A.foldl (+) zero $ A.zipWith (*) v1 v2 --}
{-- instance chi2Dim3 :: Chi2 Dim3 where --}
{--   chi2 v c = x where --}
{--     mv = toMatrix v --}
{--     mc = toMatrix c --}
{--     mx = M.transpose mv * mc * mv --}
{--     x = uidx (M.toArray mx) 0 --}
{--   sandwichvv (Vec {v:v1}) (Vec {v:v2}) = vv where --}
{--     vv = A.foldl (+) zero $ A.zipWith (*) v1 v2 --}

-------------------------------------------------------------------------
-- TransMat2 for operations that need two dimensional parameters
-- that is, between Jac and Cov or Vec
-- operators follow convetion of | for Vec, * for Cov and || for Jac
--   e.g. ||* to multiply Jac * Cov, and ||*||  for Jac^T * Cov * Jac
class TransMat2 a b where
  sandwich :: Jac a b -> Cov a -> Cov b        -- Jac^T * Cov * Jac
  transcov :: Jac a b -> Cov b -> Jac a b      -- Jac * Cov
  transvoc :: Cov a -> Jac a b -> Jac a b      -- Cov * Jac
  transvec :: Jac a b -> Vec b -> Vec a        -- Jac * Vec
  sandwichjj :: Jac a b -> Jac b a -> Jac a a  -- Jac * Jac
  {-- tr :: Jac a b -> Jac b a --}
infixr 7 sandwich as ||*||
infixr 7 transcov as ||*
infixr 7 transvoc as *||
infixr 7 transvec as |||
infixr 7 sandwichjj as ||||

instance tmat33 :: TransMat2 Dim3 Dim3 where
  sandwich j c = c' where
    mj = toMatrix j
    mc = toMatrix c
    mc' = M.transpose mj * mc * mj
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
  {-- tr j = jt where --}
  {--   mj = toMatrix j --}
  {--   mjt = M.transpose mj --}
  {--   jt = fromArray $ M.toArray mjt --}
instance tmat34 :: TransMat2 Dim3 Dim4 where
  sandwich j c = c' where
    mj = toMatrix j
    mc = toMatrix c
    mc' = M.transpose mj * mc * mj
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
  {-- tr j = jt where --}
  {--   mj = toMatrix j --}
  {--   mjt = M.transpose mj --}
  {--   jt = fromArray $ M.toArray mjt --}
instance tmat35 :: TransMat2 Dim3 Dim5 where
  sandwich j c = c' where
    mj = toMatrix j
    mc = toMatrix c
    mc' = M.transpose mj * mc * mj
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
    c'  = fromArray $ M.toArray mc'
  {-- tr j = jt where --}
  {--   mj = toMatrix j --}
  {--   mjt = M.transpose mj --}
  {--   jt = fromArray $ M.toArray mjt --}
instance tmat43 :: TransMat2 Dim4 Dim3 where
  sandwich j c = c' where
    mj = toMatrix j
    mc = toMatrix c
    mc' = M.transpose mj * mc * mj
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
  {-- tr j = jt where --}
  {--   mj = toMatrix j --}
  {--   mjt = M.transpose mj --}
  {--   jt = fromArray $ M.toArray mjt --}
instance tmat53 :: TransMat2 Dim5 Dim3 where
  sandwich j c = c' where
    mj = toMatrix j
    mc = toMatrix c
    mc' = M.transpose mj * mc * mj
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
  {-- tr (Jac {v}) = Jac {v:v'} where --}
  {--   a11 = unsafePartial $ A.unsafeIndex v 0 --}
  {--   a12 = unsafePartial $ A.unsafeIndex v 1 --}
  {--   a13 = unsafePartial $ A.unsafeIndex v 2 --}
  {--   a21 = unsafePartial $ A.unsafeIndex v 3 --}
  {--   a22 = unsafePartial $ A.unsafeIndex v 4 --}
  {--   a23 = unsafePartial $ A.unsafeIndex v 5 --}
  {--   a31 = unsafePartial $ A.unsafeIndex v 6 --}
  {--   a32 = unsafePartial $ A.unsafeIndex v 7 --}
  {--   a33 = unsafePartial $ A.unsafeIndex v 8 --}
  {--   a41 = unsafePartial $ A.unsafeIndex v 9 --}
  {--   a42 = unsafePartial $ A.unsafeIndex v 10 --}
  {--   a43 = unsafePartial $ A.unsafeIndex v 11 --}
  {--   a51 = unsafePartial $ A.unsafeIndex v 12 --}
  {--   a52 = unsafePartial $ A.unsafeIndex v 13 --}
  {--   a53 = unsafePartial $ A.unsafeIndex v 14 --}
  {--   v' = [a11,a21,a31,a41,a51,a12,a22,a32,a42,a52,a13,a23,a33,a43,a53] --}
instance tmat55 :: TransMat2 Dim5 Dim5 where
  sandwich j c = c' where
    mj = toMatrix j
    mc = toMatrix c
    mc' = M.transpose mj * mc * mj
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
  {-- tr j = jt where --}
  {--   mj = toMatrix j --}
  {--   mjt = M.transpose mj --}
  {--   jt = fromArray $ M.toArray mjt --}
-------------------------------------------------------------------------
-- TransMat for operations that need only dimensional parameters
-- TransMat for Cov * Vec and Cov * Cov * Cov
--
class TransMat a where
  ttransvec :: Cov a -> Vec a -> Vec a      -- Cov * Vec
  multiplycc :: Cov a -> Cov a -> Jac a a     -- Cov * Cov'
  sandwichcc :: Cov a -> Cov a -> Cov a     -- Cov * Cov' * Cov
infixr 7 ttransvec as *|
infixr 7 multiplycc as **
infixr 7 sandwichcc as ***
instance tvec3 :: TransMat Dim3 where
  ttransvec c v = v' where
    mc = toMatrix c
    mv = toMatrix v
    mv' = mc * mv
    v' = fromArray $ M.toArray mv'
  sandwichcc c1 c2 = c' where
    mc1 = toMatrix c1
    mc2 = toMatrix c2
    mc' = mc1 * mc2 * mc1
    c' = fromArray $ M.toArray mc'
  multiplycc c1 c2 = j where
    mc1 = toMatrix c1
    mc2 = toMatrix c2
    mj = mc1 * mc2
    j = fromArray $ M.toArray mj
instance tvec4 :: TransMat Dim4 where
  ttransvec c v = v' where
    mc = toMatrix c
    mv = toMatrix v
    mv' = mc * mv
    v' = fromArray $ M.toArray mv'
  sandwichcc c1 c2 = c' where
    mc1 = toMatrix c1
    mc2 = toMatrix c2
    mc' = mc1 * mc2 * mc1
    c' = fromArray $ M.toArray mc'
  multiplycc c1 c2 = j where
    mc1 = toMatrix c1
    mc2 = toMatrix c2
    mj = mc1 * mc2
    j = fromArray $ M.toArray mj
instance tvec5 :: TransMat Dim5 where
  ttransvec c v = v' where
    mc = toMatrix c
    mv = toMatrix v
    mv' = mc * mv
    v' = fromArray $ M.toArray mv'
  sandwichcc c1 c2 = c' where
    mc1 = toMatrix c1
    mc2 = toMatrix c2
    mc' = mc1 * mc2 * mc1
    c' = fromArray $ M.toArray mc'
  multiplycc c1 c2 = j where
    mc1 = toMatrix c1
    mc2 = toMatrix c2
    mj = mc1 * mc2
    j = fromArray $ M.toArray mj

scaleDiag :: Number -> Cov3 -> Cov3
scaleDiag s (Cov {v}) = (Cov {v: v'}) where
  a11 = s * (uidx v 0)
  a22 = s * (uidx v 3)
  a33 = s * (uidx v 5)
  v' = [a11, 0.0, 0.0, a22,0.0, a33]

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


-- CHOLESKY DECOMPOSITION

-- | Simple Cholesky decomposition of a symmetric, positive definite matrix.
--   The result for a matrix /M/ is a lower triangular matrix /L/ such that:
--
--   * /M = LL^T/.
--
--   Example:
--
-- >            (  2 -1  0 )   (  1.41  0     0    )
-- >            ( -1  2 -1 )   ( -0.70  1.22  0    )
-- > choldx     (  0 -1  2 ) = (  0.00 -0.81  1.15 )
--
-- Given a positive-deﬁnite symmetric matrix a[1..n][1..n],
-- this routine constructs its Cholesky decomposition,
-- A = L · L^T
-- The Cholesky factor L is returned in the lower triangle of a,
-- except for its diagonal elements which are returned in p[1..n].


choldc :: forall a. Cov a -> Int -> Jac a a
choldc (Cov {v: a}) n = Jac {v: a'} where
  w = n
  ll = n*n --n*(n+1)/2
  idx :: Int -> Int -> Int
  idx i j | i <= j    = ((i-1)*w - (i-1)*(i-2)/2 + j-i)
          --| otherwise = ((j-1)*w - (j-1)*(j-2)/2 + i-j)
          | otherwise = error "idx: i < j"
  idx' :: Int -> Int -> Int
  idx' j i | i >= j   = (i-1)*w + j-1
           | otherwise = error "idx': i < j"
  {-- run :: forall a. (forall h. Eff (st :: ST h) (STArray h a)) -> Array a --}
  {-- run act = pureST (act >>= unsafeFreeze) --}
  {-- a' = run (do --}
  a' = A.take (n*n) $ pureST ((do
    -- make a STArray of n x n + space for diagonal
    arr <- emptySTArray
    _ <- pushAllSTArray arr (A.replicate (ll+n) 0.0)

    -- loop over input array using Numerical Recipies algorithm (chapter 2.9)
    forE 1 (w+1) \i -> do
      forE i (w+1) \j -> do
          _ <- pokeSTArray arr (idx' i j) (uidx a (idx i j))
          let kmin = 1
              kmax = (i-1) + 1
          forE kmin kmax \k -> do
              aik <- peekSTArray arr (idx' k i)
              ajk <- peekSTArray arr (idx' k j)
              sum <- peekSTArray arr (idx' i j)
              void $ pokeSTArray arr (idx' i j) ((uJust sum)
                                               - (uJust aik) * (uJust ajk))

          msum <- peekSTArray arr (idx' i j)
          let sum' = uJust msum
              sum = if (i==j) && sum' < 0.0
                       then error ("choldc: not a positive definite matrix " <> show a)
                       else sum'
          mp_i' <- peekSTArray arr (ll+i-1)
          let p_i' = uJust mp_i'
              p_i = if i == j then sqrt sum else p_i'
          void $ if i==j
                         then pokeSTArray arr (ll+i-1) p_i -- store diag terms outside main array
                         else pokeSTArray arr (idx' i j) (sum/p_i)
          pure $ unit

    -- copy diagonal back into array
    forE 1 (w+1) \i -> do
          maii <- peekSTArray arr (ll+i-1)
          let aii = uJust maii
          void $ pokeSTArray arr (idx' i i) aii
    pure arr) >>= unsafeFreeze)

-- | Matrix inversion using Cholesky decomposition
-- | based on Numerical Recipies formula in 2.9
--
cholInv :: forall a. Cov a -> Int -> Cov a
cholInv (Cov {v: a}) n = Cov {v: a'} where
  ll = n*n --n*(n+1)/2
  idx :: Int -> Int -> Int -- index into values array of symmetric matrices
  idx i j | i <= j    = ((i-1)*n - (i-1)*(i-2)/2 + j-i)
          | otherwise = ((j-1)*n - (j-1)*(j-2)/2 + i-j)
  idx' :: Int -> Int -> Int -- index into values array for full matrix
  idx' i j = (i-1)*n + j-1
  l = pureST ((do
    -- make a STArray of n x n + space for diagonal +1 for summing
    arr <- emptySTArray
    void $ pushAllSTArray arr (A.replicate (ll+n+1) 0.0)

    -- loop over input array using Numerical Recipies algorithm (chapter 2.9)
    forE 1 (n+1) \i -> do
      forE i (n+1) \j -> do
          let aij = uidx a (idx i j)
          void $ if i==j then pokeSTArray arr (ll+i-1) aij
                         else pokeSTArray arr (idx' j i) aij
          forE 1 i \k -> do
              maik <- peekSTArray arr (idx' i k)
              majk <- peekSTArray arr (idx' j k)
              maij <- if i==j then peekSTArray arr (ll+i-1)
                              else peekSTArray arr (idx' j i)
              let sum = (uJust maij) - (uJust maik) * (uJust majk)
              void $ if i==j then pokeSTArray arr (ll+i-1) sum
                             else pokeSTArray arr (idx' j i) sum
          msum <- if i==j then peekSTArray arr (ll+i-1)
                          else peekSTArray arr (idx' j i)
          let sum' = uJust msum
              sum = if i==j && sum' < 0.0
                       then error ("choldInv: not a positive definite matrix "
                                    <> show a)
                       else sum'
          mp_i' <- peekSTArray arr (ll+i-1)
          let p_i' = uJust mp_i'
              p_i = if i == j then sqrt sum else p_i'
          void $ if i==j then pokeSTArray arr (ll+i-1) p_i
                         else pokeSTArray arr (idx' j i) (sum/p_i)
          pure $ unit

    -- invert L -> L^(-1)
    forE 1 (n+1) \i -> do
      mp_i <- peekSTArray arr (ll+i-1)
      void $ pokeSTArray arr (idx' i i) (1.0/(uJust mp_i))
      forE (i+1) (n+1) \j -> do
        void $ pokeSTArray arr (ll+n) 0.0
        forE i j \k -> do
          majk <- peekSTArray arr (idx' j k)
          maki <- peekSTArray arr (idx' k i)
          sum <- peekSTArray arr (ll+n)
          void $ pokeSTArray arr (ll+n)
                    ((uJust sum) - (uJust majk) * (uJust maki))
        msum <- peekSTArray arr (ll+n)
        mp_j <- peekSTArray arr (ll+j-1)
        void $ pokeSTArray arr (idx' j i) ((uJust msum)/(uJust mp_j))
    pure arr) >>= unsafeFreeze)
  a' = do
    i <- A.range 1 n
    j <- A.range i n
    let aij = sum do
                  k <- A.range 1 n
                  pure $ (uidx l (idx' k i)) * (uidx l (idx' k j))
    pure $ aij

--C version Numerical Recipies 2.9
--for (i=1;i<=n;i++) {
--  for (j=i;j<=n;j++) {
--    for (sum=a[i][j],k=i-1;k>=1;k--) sum -= a[i][k]*a[j][k];
--    if (i == j) {
--      if (sum <= 0.0) nrerror("choldc failed");
--      p[i]=sqrt(sum);
--    } else a[j][i]=sum/p[i];
--  }
--}
-- In this, and many other applications, one often needs L^(−1) . The lower 
-- triangle of this matrix can be efﬁciently found from the output of choldc:
--for (i=1;i<=n;i++) {
--  a[i][i]=1.0/p[i];
--  for (j=i+1;j<=n;j++) {
--    sum=0.0;
--    for (k=i;k<j;k++) sum -= a[j][k]*a[k][i];
--    a[j][i]=sum/p[j];
--  }
--}
