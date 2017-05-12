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
import Data.Maybe (Maybe (..))
import Data.Monoid.Additive (Additive)
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
    a22 = unsafePartial $ A.unsafeIndex a 4
    a23 = unsafePartial $ A.unsafeIndex a 5
    a24 = unsafePartial $ A.unsafeIndex a 6
    a33 = unsafePartial $ A.unsafeIndex a 7
    a34 = unsafePartial $ A.unsafeIndex a 8
    a44 = unsafePartial $ A.unsafeIndex a 9
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
  inv a = a
  invMaybe a = Just a
  det a = 1.0
  diag (Cov {v}) = a where
    a11 = unsafePartial $ A.unsafeIndex v 0
    a22 = unsafePartial $ A.unsafeIndex v 4
    a33 = unsafePartial $ A.unsafeIndex v 8
    a = [a11,a22,a33]
instance symMatCov4 :: SymMat (Cov Dim4) where
  inv a = a
  invMaybe a = Just a
  det a = 1.0
  diag (Cov {v}) = a where
    a11 = unsafePartial $ A.unsafeIndex v 0
    a22 = unsafePartial $ A.unsafeIndex v 5
    a33 = unsafePartial $ A.unsafeIndex v 10
    a44 = unsafePartial $ A.unsafeIndex v 15
    a = [a11,a22,a33,a44]
instance symMatCov5 :: SymMat (Cov Dim5) where
  inv a = a
  invMaybe a = Just a
  det a = 1.0
  diag (Cov {v}) = a where
    a11 = unsafePartial $ A.unsafeIndex v 0
    a22 = unsafePartial $ A.unsafeIndex v 6
    a33 = unsafePartial $ A.unsafeIndex v 12
    a44 = unsafePartial $ A.unsafeIndex v 18
    a55 = unsafePartial $ A.unsafeIndex v 24
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
   --   , a13*b11 + a23*b13 + a33*b13 + a34*b14 + a35*b15
   --   , a13*b12 + a23*b23 + a33*b23 + a34*b24 + a35*b25
        , a13*b13 + a23*b33 + a33*b33 + a34*b34 + a35*b35
        , a13*b14 + a23*b34 + a33*b34 + a34*b44 + a35*b45
        , a13*b15 + a23*b35 + a33*b35 + a34*b45 + a35*b55
   --   , a14*b11 + a24*b14 + a34*b14 + a44*b14 + a45*b15
   --   , a14*b12 + a24*b24 + a34*b24 + a44*b24 + a45*b25
   --   , a14*b13 + a24*b34 + a34*b34 + a44*b34 + a45*b35
        , a14*b14 + a24*b44 + a34*b44 + a44*b44 + a45*b45
        , a14*b15 + a24*b45 + a34*b45 + a44*b45 + a45*b55
   --   , a15*b11 + a25*b15 + a35*b15 + a45*b14 + a55*b15
   --   , a15*b12 + a25*b25 + a35*b25 + a45*b24 + a55*b25
   --   , a15*b13 + a25*b35 + a35*b35 + a45*b34 + a55*b35
   --   , a15*b14 + a25*b45 + a35*b45 + a45*b44 + a55*b45
        , a15*b15 + a25*b55 + a35*b55 + a45*b45 + a55*b55
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
  a22 = unsafePartial $ A.unsafeIndex v 6
  a23 = unsafePartial $ A.unsafeIndex v 7
  a33 = unsafePartial $ A.unsafeIndex v 13
  v' = [a11,a12,a13,a22,a23,a33]


newtype MD = MakeMD {m3 :: Cov3, m5 :: Cov5}
instance showMD :: Show MD where
  show (MakeMD {m3, m5}) = "Show MD,\nm3=" <> show m3 <> "\nm5=" <> show m5

testCov = "testCov: " <> show md <> "\n"
                      <> show mm3
                      <> show mm5
                      <> "exp v3 " <> show ( (v3 + v3) |.| v3 ) <> "\n"
                      <> "tj3 " <> show tj3 <> "vv3 " <> show vv3
                      <> show (v3 |*| c3) where
  c3 :: Cov3
  c3 = fromArray [1.0,2.0,3.0,4.0,5.0,6.0]
  c5 :: Cov5
  c5 = fromArray [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0]
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

