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
  , multStd
  , fromArray, fromArray2, zero_, identity
  ) as M
import Stuff

newtype Dim3 = Dim3 Int
newtype Dim5 = Dim5 Int
newtype Dim53 = Dim53 Int

newtype Cov a = Cov { v :: Array Number }
newtype Jac a b = Jac { v :: Array Number }
newtype Vec a = Vec { v :: Array Number }

class Mat a where
  val :: a -> Array Number
  fromArray :: Array Number -> a
  toMatrix :: a -> M.Matrix Number
instance matCov3 :: Mat (Cov Dim3) where
  val (Cov {v}) = v
  fromArray a | A.length a /= 6 =
                  error $ "Cov3 fromArray: wrong input array length " <> show (A.length a)
              | otherwise = Cov {v: a}
  toMatrix (Cov {v}) = M.fromArray2 3 3 vv where
    a11 = unsafePartial $ A.unsafeIndex v 0
    a12 = unsafePartial $ A.unsafeIndex v 1
    a13 = unsafePartial $ A.unsafeIndex v 2
    a22 = unsafePartial $ A.unsafeIndex v 3
    a23 = unsafePartial $ A.unsafeIndex v 4
    a33 = unsafePartial $ A.unsafeIndex v 5
    vv = [a11, a12, a13, a12, a22, a23, a13, a23, a33]
instance matCov5 :: Mat (Cov Dim5) where
  val (Cov {v}) = v
  fromArray a | A.length a /= 15 =
                  error "Cov5 fromArray: wrong input array length"
              | otherwise = Cov {v: a}
  toMatrix (Cov {v}) = M.fromArray2 5 5 vv where
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
instance matVec3 :: Mat (Vec Dim3) where
  val (Vec {v}) = v
  fromArray a | A.length a /= 3 =
                  error "Vec3 fromArray: wrong input array length"
              | otherwise = Vec {v: a}
  toMatrix (Vec {v}) = M.fromArray 3 v
instance matVec5 :: Mat (Vec Dim5) where
  val (Vec {v}) = v
  fromArray a | A.length a /= 5 =
                  error "Vec5 fromArray: wrong input array length"
              | otherwise = Vec {v: a}
  toMatrix (Vec {v}) = M.fromArray 5 v
instance matJac53 :: Mat (Jac Dim5 Dim3) where
  val (Jac {v}) = v
  fromArray a | A.length a /= 15 =
                  error "Jac53 fromArray: wrong input array length"
              | otherwise = Jac {v: a}
  toMatrix (Jac {v}) = M.fromArray2 5 3 v
class SymMat a where
  inv :: a -> a
  invMaybe :: a -> Maybe a
  det :: a -> Number
instance symMatCov3 :: SymMat (Cov Dim3) where
  inv a = a
  invMaybe a = Just a
  det a = 1.0
instance symMatCov5 :: SymMat (Cov Dim5) where
  inv a = a
  invMaybe a = Just a
  det a = 1.0
type Cov3 = Cov Dim3
type Cov5 = Cov Dim5

type Jac53 = Jac Dim5 Dim3
type Jacos = {aa :: Jac53, bb :: Jac53, h0 :: Vec5}

type Vec3 = Vec Dim3
type Vec5 = Vec Dim5

sw5 :: Jac53 -> Cov3 -> Cov5
sw5 j c = one
sw3 :: Jac53 -> Cov5 -> Cov3
sw3 j c = one
swv5 :: Jac53 -> Vec3 -> Vec5
swv5 j v = zeroVec5


sw1 :: Vec3 -> Cov3 -> Number
sw1 v c = 1.0

tr :: Jac53 -> Jac53
tr j = j

instance semiringCov3 :: Semiring (Cov Dim3) where
  add (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (+) v1 v2}
  zero = Cov {v: A.replicate 6 0.0 }
  mul (Cov {v: v1}) (Cov {v: v2}) = Cov {v: cov3StdMult v1 v2}
  one = Cov { v: [1.0, 0.0, 0.0, 1.0, 0.0, 1.0] }
instance ringCov3 :: Ring (Cov Dim3) where
  sub (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (-) v1 v2}
instance showCov3 :: Show (Cov Dim3) where
  show (Cov {v}) = "Show (Cov Dim3) \n" <> (show $ cov3Matrix v)

cov3StdMult :: Array Number -> Array Number -> Array Number
cov3StdMult a b = c where
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
cov3Matrix :: Array Number -> M.Matrix Number
cov3Matrix v = M.fromArray2 3 3 m where
  a11 = unsafePartial $ A.unsafeIndex v 0
  a12 = unsafePartial $ A.unsafeIndex v 1
  a13 = unsafePartial $ A.unsafeIndex v 2
  a22 = unsafePartial $ A.unsafeIndex v 3
  a23 = unsafePartial $ A.unsafeIndex v 4
  a33 = unsafePartial $ A.unsafeIndex v 5
  m = [a11, a12, a13, a12, a22, a23, a13, a23, a33]

instance semiringCov5 :: Semiring (Cov Dim5) where
  add (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (+) v1 v2}
  zero = Cov {v: A.replicate 15 0.0 }
  mul (Cov {v: v1}) (Cov {v: v2}) = Cov {v: cov5StdMult v1 v2}
  one = Cov { v: [1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0
            , 0.0, 0.0, 1.0, 0.0, 1.0] }
instance ringCov5 :: Ring (Cov Dim5) where
  sub (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (-) v1 v2}
instance showCov5 :: Show (Cov Dim5) where
  show (Cov {v}) = "Show (Cov Dim5) \n" <> (show $ cov5Matrix v)

cov5StdMult :: Array Number -> Array Number -> Array Number
cov5StdMult a b = c where
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
cov5Matrix :: Array Number -> M.Matrix Number
cov5Matrix v = M.fromArray2 5 5 m where
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
  m = [ a11, a12, a13, a14, a15
      , a12, a22, a23, a24, a25
      , a13, a23, a33, a34, a35
      , a14, a24, a34, a44, a45
      , a15, a25, a35, a45, a55
      ]

-- Instances for Vec
-- almost semiring
addVec :: forall a.  Vec a -> Vec a -> Vec a
addVec (Vec {v: v1}) (Vec {v: v2}) = Vec {v: A.zipWith (+) v1 v2}
zeroVec3 :: Vec3
zeroVec3 = Vec {v: A.replicate 3 0.0 }
zeroVec5 :: Vec5
zeroVec5 = Vec {v: A.replicate 5 0.0 }
mulVec :: forall a. Vec a ->  Vec a -> Vec a
mulVec (Vec {v: v1}) (Vec {v: v2}) = Vec {v: A.singleton $ A.foldl (+) zero $ A.zipWith (*) v1 v2}
scalarVec :: forall a. Vec a -> Number
scalarVec (Vec {v}) = unsafePartial $ AP.head v
oneVec3 :: Vec3
oneVec3 = Vec { v: A.replicate 3 1.0 }
oneVec5 :: Vec5
oneVec5 = Vec { v: A.replicate 5 1.0 }
subVec :: forall a. Vec a -> Vec a -> Vec a
subVec (Vec {v: v1}) (Vec {v: v2}) = Vec {v: A.zipWith (-) v1 v2}
instance showVec3 :: Show (Vec Dim3) where
  show (Vec {v}) = "Show (Vec Dim3) \n" <> (show $ M.fromArray 3 v)
instance showVec5 :: Show (Vec Dim5) where
  show (Vec {v}) = "Show (Vec Dim5) \n" <> (show $ M.fromArray 5 v)

class Chi2 a where
  chi2 :: Vec a -> Cov a -> Number

instance chi2Dim5 :: Chi2 Dim5 where
  chi2 v c = 5.0
instance chi2Dim3 :: Chi2 Dim3 where
  chi2 v c = 3.0

class TMat a b where
  sandwich ::  Jac a b -> Cov a -> Cov b
infixr 6 sandwich as |*|

instance tmat35 :: TMat Dim3 Dim5 where
  sandwich j c = fromArray [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0]
  {-- (|*|) c j = c --}
instance tmat53 :: TMat Dim5 Dim3 where
  sandwich j c = fromArray [1.0,2.0,3.0,4.0,5.0,6.0]

class TVec a b where
  transvec :: Jac a b -> Vec b -> Vec a
infixr 6 transvec as *|

instance tvec35 :: TVec Dim3 Dim5 where
  transvec j v = fromArray [1.0, 2.0, 3.0]
instance tvec53 :: TVec Dim5 Dim3 where
  transvec j v = fromArray [1.0, 2.0, 3.0, 4.0, 5.0]


class TTVec a where
  ttransvec :: Cov a -> Vec a -> Vec a



newtype MD = MakeMD {m3 :: Cov3, m5 :: Cov5}
instance showMD :: Show MD where
  show (MakeMD {m3, m5}) = "Show MD,\nm3=" <> show m3 <> "\nm5=" <> show m5

testCov = "testCov: " <> show md <> "\n"
                      <> show mm3
                      <> show mm5
                      <> show (scalarVec (mulVec (addVec v3 v3)  v3))  
                      <> show tj3
                      <> show (chi2 v3 c3) where
  c3 :: Cov3
  c3 = fromArray [1.0,2.0,3.0,4.0,5.0,6.0]
  c5 :: Cov5
  c5 = fromArray [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0]
  v3 :: Vec3
  v3 = fromArray [10.0,11.0,12.0]
  j53 :: Jac53
  j53 = fromArray [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0]
  tj3 :: Cov3
  tj3 = j53 |*| c5

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

