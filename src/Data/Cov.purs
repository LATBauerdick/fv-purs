module Data.Cov
    where

import Prelude
import Prelude.Extended (
    error, fromIntegral
  , fromList, prettyMatrix, range, uJust, uidx, undefined
  )

import Data.Array (
    replicate, length, foldl
  , zipWith, take
  ) as A
import Data.Foldable ( sum )
import Data.Maybe ( Maybe (..) )
import Control.MonadZero (guard)
import Data.Int ( floor )
import Math ( abs, sqrt )
import Partial.Unsafe ( unsafePartial )

import Data.Chol ( doCholdc, doCholInv )

newtype Cov a = Cov { v :: Array Number }
newtype Jac a b = Jac { v :: Array Number, nr :: Int }
newtype Vec a = Vec { v :: Array Number }

newtype Dim3 = DDim3 Int
newtype Dim4 = DDim4 Int
newtype Dim5 = DDim5 Int
type Cov3 = Cov Dim3
type Cov4 = Cov Dim4
type Cov5 = Cov Dim5
type Jac53 = Jac Dim5 Dim3
type Jac33 = Jac Dim3 Dim3
type Jac34 = Jac Dim3 Dim4
type Jac35 = Jac Dim3 Dim5
type Jac44 = Jac Dim4 Dim4
type Jac55 = Jac Dim5 Dim5
type Vec3 = Vec Dim3
type Vec4 = Vec Dim4
type Vec5 = Vec Dim5
type Jacs = {aa :: Jac53, bb :: Jac53, h0 :: Vec5}

-- access to arrays of symmetrical matrices
indV :: Int -> Int -> Int -> Int
indV w i0 j0 = (i0*w+j0)
indVs :: Int -> Int -> Int -> Int
indVs w i0 j0 | i0 <= j0   = (i0*w - (i0*(i0-1)) `div` 2 + j0-i0)
              | otherwise = (j0*w - (j0*(j0-1)) `div` 2 + i0-j0)
-------------------------------------------------------------------------
-------------------------------------------------------------------------
-------------------------------------------------------------------------
-------------------------------------------------------------------------
-- Mat to give behavior to Cov and Vec and Jac
-- ability to convert to and from Matrix and Array
-- while keeping info about dimensionality
-- also define Semiring and Ring functions
--

class Mat a where
  val :: a -> Array Number
  toArray :: a -> Array Number
  elementwise :: (Number -> Number -> Number) -> a -> a -> a

instance matCova :: Mat (Cov a) where
  val (Cov {v}) = v
  toArray c@(Cov {v}) = v' where
    l = A.length v
    n = case l of
      6  -> 3
      10 -> 4
      15 -> 5
      _  -> error $ "matCova: toArray not supported for length " <> show l
    iv = indVs n
    v' = fromList $ do
      i0 <- range 0 (n-1)
      j0 <- range 0 (n-1)
      pure $ uidx v (iv i0 j0)
  elementwise f (Cov {v: va}) (Cov {v: vb}) = (Cov {v: vc}) where
    vc = A.zipWith f va vb
instance matVeca :: Mat (Vec a) where
  val (Vec {v}) = v
  toArray (Vec {v}) = v
  elementwise f (Vec {v: va}) (Vec {v: vb}) = (Vec {v: vc}) where
    vc = A.zipWith f va vb
instance matJacab :: Mat (Jac a b) where
  val (Jac {v}) = v
  toArray (Jac {v}) = v
  elementwise f (Jac {v: va}) (Jac {v: vb, nr: r}) = (Jac {v: vc, nr: r}) where
    vc = A.zipWith f va vb

class ShowMat a where
  showMatrix :: a -> String
instance showCova :: ShowMat (Cov a) where
  showMatrix (Cov {v}) = let
    makeSymMat :: Int -> Array Number -> Array Number
    makeSymMat n vs = fromList $ do
      let iv = indVs n
      i <- range 0 (n-1)
      j <- range 0 (n-1)
      pure $ uidx vs (iv i j)
    in
      case A.length v of
                            6  -> prettyMatrix 3 3 $ makeSymMat 3 v
                            10 -> prettyMatrix 4 4 $ makeSymMat 4 v
                            15 -> prettyMatrix 5 5 $ makeSymMat 5 v
                            _ -> error $ "showCova showMatrix "
                                          <> show (A.length v)
else instance showVeca :: ShowMat (Vec a) where
  showMatrix (Vec {v}) = prettyMatrix (A.length v) 1 v
else instance showJac53 :: ShowMat (Jac Dim5 Dim3) where
  showMatrix (Jac {v}) = prettyMatrix 5 3 v
else instance showJac35 :: ShowMat (Jac Dim3 Dim5) where
  showMatrix (Jac {v}) = prettyMatrix 3 5 v
else instance showJacaa :: ShowMat (Jac a b) where
  showMatrix j@(Jac {v, nr: r}) = prettyMatrix r ((A.length v) `div` r) v

class ArrMat a where
  fromArray :: Array Number -> a
instance arrMatVeca :: ArrMat (Vec a) where
  fromArray a = Vec {v: a}
instance arrMatCov3 :: ArrMat (Cov Dim3) where
  fromArray a = c' where
    l = A.length a
    c' = case l of
      6   -> Cov {v: a}
      _   -> Cov {v: let
          n = floor <<< sqrt <<< fromIntegral $ l
          iv = indV n
        in fromList $ do -- only upper triangle
          i0 <- range 0 (n-1)
          j0 <- range i0 (n-1)
          pure $ uidx a (iv i0 j0) }
instance arrMatCov4 :: ArrMat (Cov Dim4) where
  fromArray a = c' where
    l = A.length a
    c' = case l of
      10  -> Cov {v: a}
      _   -> Cov {v: let
          n = floor <<< sqrt <<< fromIntegral $ l
          iv = indV n
        in fromList $ do -- only upper triangle
          i0 <- range 0 (n-1)
          j0 <- range i0 (n-1)
          pure $ uidx a (iv i0 j0) }
instance arrMatCov5 :: ArrMat (Cov Dim5) where
  fromArray a = c' where
    l = A.length a
    c' = case l of
      15  -> Cov {v: a}
      _   -> Cov {v: let
          n = floor <<< sqrt <<< fromIntegral $ l
          iv = indV n
        in fromList $ do -- only upper triangle
          i0 <- range 0 (n-1)
          j0 <- range i0 (n-1)
          pure $ uidx a (iv i0 j0) }

-----------------------------------------------------------------
-- | functions for symetric matrices: Cov
-- | type class SymMat
class SymMat a where
  inv :: Cov a -> Cov a                -- | inverse matrix
  invMaybe :: Cov a -> Maybe (Cov a)   -- | Maybe inverse matrix
  det :: Cov a -> Number               -- | determinant
  diag :: Cov a -> Array Number        -- | Array of diagonal elements

instance symMatCov3 :: SymMat Dim3 where
--  inv m | trace ( "inv " <> (show m) ) False = undefined
  inv m = m' where
    mm = invMaybe m
    m' = case mm of
           Nothing -> error $ "can't invert 3x3 matrix " <> show m
           Just x -> x
  invMaybe (Cov {v}) = _inv v where
    _inv :: Array Number -> Maybe (Cov Dim3)
    _inv = unsafePartial $ \[a11,a12,a13,a22,a23,a33] -> do
      let det = (a33*a12*a12 - 2.0*a13*a23*a12 + a13*a13*a22
                + a11*(a23*a23 - a22*a33))
      guard $ (abs det) > 1.0e-50
      let
          b11 = (a23*a23 - a22*a33)/det
          b12 = (a12*a33 - a13*a23)/det
          b13 = (a13*a22 - a12*a23)/det
          b22 = (a13*a13 - a11*a33)/det
          b23 = (a11*a23 - a12*a13)/det
          b33 = (a12*a12 - a11*a22)/det
          v' = [b11,b12,b13,b22,b23,b33]
      pure $ Cov {v: v'}
  det (Cov {v}) = _det v where
    _det :: Array Number -> Number
    _det = unsafePartial $ \[a,b,c,d,e,f] ->
            a*d*f - a*e*e - b*b*f + 2.0*b*c*e - c*c*d
  diag (Cov {v}) = _diag v where
    _diag :: Array Number -> Array Number
    _diag = unsafePartial $ \[a11,_,_,a22,_,a33] -> [a11,a22,a33]
instance symMatCov4 :: SymMat Dim4 where
  inv m = uJust (invMaybe m)
  invMaybe (Cov {v}) = _inv v where
    _inv :: Array Number -> Maybe (Cov Dim4)
    _inv = unsafePartial $ \[a,b,c,d,e,f,g,h,i,j] -> do
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
      pure $ fromArray [a',b',c',d',e',f',g',h',i',j']
  det (Cov {v}) = _det v where
    _det :: Array Number -> Number
    _det = unsafePartial $ \[a,b,c,d,e,f,g,h,i,j] ->
        (a*e*h*j - a*e*i*i - a*f*f*j + 2.0*a*f*g*i - a*g*g*h
          - b*b*h*j + b*b*i*i - 2.0*d*(b*f*i - b*g*h - c*e*i + c*f*g)
          + b*c*(2.0*f*j - 2.0*g*i) + c*c*(g*g - e*j) + d*d*(f*f - e*h))
  diag (Cov {v}) = _diag v where
    _diag :: Array Number -> Array Number
    _diag = unsafePartial $ \[a11,_,_,_,a22,_,_,a33,_,a44] -> [a11,a22,a33,a44]
instance symMatCov5 :: SymMat Dim5 where
  inv m = cholInv m
  invMaybe m = Just (cholInv m)
  det (Cov {v}) = _det v where
    _det :: Array Number -> Number
    _det = unsafePartial $ \[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o] ->
      a*f*j*m*o - a*f*j*n*n - a*f*k*k*o + 2.0*a*f*k*l*n - a*f*l*l*m
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
  diag (Cov {v}) = _diag v where
    _diag :: Array Number -> Array Number
    _diag = xxx where
      xxx = case A.length v of
              15 -> unsafePartial $ \[a,_,_,_,_,b,_,_,_,c,_,_,d,_,e] -> [a,b,c,d,e]
              _  -> error $ "symMatCov5 diag: wrong length of Cov v " <> show (A.length v)

class MulMat a b c | a b -> c where
  mulm :: a -> b -> c
infixr 7 mulm as *.
instance mulMata :: MulMat (Cov a) (Cov a) (Jac a a) where
  mulm (Cov {v: va}) (Cov {v: vb}) = Jac {v: vc, nr: na} where
    na = case A.length va of
              6  -> 3
              10 -> 4
              15 -> 5
              _  -> 0 -- error $ "mulMatCC wrong length of Cov v " <> show (A.length va)
    vc = fromList $ do
      let ixa = indVs na
          ixb = indVs na
      i0 <- range 0 (na-1)
      j0 <- range 0 (na-1)
      pure $ sum $ do
                  k0 <- range 0 (na-1)
                  pure $ (uidx va (ixa i0 k0)) * (uidx vb (ixb k0 j0))
instance mulMatJC :: MulMat (Jac a b) (Cov b) (Jac a b) where
  mulm j@(Jac {v: va, nr: r}) c@(Cov {v: vb}) = Jac {v: vc, nr: r} where
    nb = case A.length vb of
              6  -> 3
              10 -> 4
              15 -> 5
              _  -> 0 -- error $ "mulMatJC wrong length of Cov v " <> show (A.length vb)
    na = (A.length va) `div` nb
    n = nb
    vc = fromList $ do
      let ixa = indV nb
          ixb = indVs nb
      i0 <- range 0 (na-1)
      j0 <- range 0 (nb-1)
      pure $ sum $ do
        k0 <- range 0 (nb-1)
        pure $ (uidx va (ixa i0 k0)) * (uidx vb (ixb k0 j0))
instance mulMatCJ :: MulMat (Cov a) (Jac a b) (Jac a b) where
  mulm c@(Cov {v: va}) j@(Jac {v: vb}) = Jac {v: vc, nr: na} where
    na = case A.length va of
              6  -> 3
              10 -> 4
              15 -> 5
              _  -> 0 -- error $ "mulMatCJ wrong length of Cov v " <> show (A.length va)
    nb = (A.length vb) `div` na
    vc = fromList $ do
      let ixa = indVs na
          ixb = indV nb
      i0 <- range 0 (na-1)
      j0 <- range 0 (nb-1)
      pure $ sum $ do
        k0 <- range 0 (na-1)
        pure $ (uidx va (ixa i0 k0)) * (uidx vb (ixb k0 j0))
instance mulMatJV :: MulMat (Jac a b) (Vec b) (Vec a) where
  mulm j@(Jac {v: va}) v@(Vec {v: vb}) = Vec {v: vc} where
    nb = A.length vb
    na = (A.length va) `div` nb
    vc = fromList $ do
      let ixa = indV nb
      i0 <- range 0 (na-1)
      pure $ sum $ do
        k0 <- range 0 (nb-1)
        pure $ (uidx va (ixa i0 k0)) * (uidx vb k0)
instance mulMatJJ :: MulMat (Jac a b) (Jac b a) (Jac a a) where
  mulm (Jac {v: va, nr: r}) (Jac {v: vb}) = Jac {v: vc, nr: r} where
    nb = case A.length va of
              12 -> 12 `div` r
              15 -> 15 `div` r
              9  -> 3
              16 -> 4
              25 -> 5
              _  -> 0 -- error $ "mulMatJJ can only do 3x5 * 5x3, 3x4 * 4*3, or squares" <> show (A.length vb)
    na = (A.length va) `div` nb
    vc = fromList $ do
      let ixa = indV nb
          ixb = indV na
      i0 <- range 0 (na-1)
      j0 <- range 0 (na-1)
      pure $ sum $ do
        k0 <- range 0 (nb-1)
        pure $ (uidx va (ixa i0 k0)) * (uidx vb (ixb k0 j0))
instance mulMatCV :: MulMat (Cov a) (Vec a) (Vec a) where
  mulm (Cov {v: va}) (Vec {v: vb}) = Vec {v: vc} where
    nb = A.length vb
    na = nb
    vc = fromList $ do
      let ixa = indVs na
      i0 <- range 0 (na-1)
      pure $ sum $ do
        k0 <- range 0 (na-1)
        pure $ (uidx va (ixa i0 k0)) * (uidx vb k0)
instance mulMatVV :: MulMat (Vec a) (Vec a) Number where
  mulm (Vec {v:va}) (Vec {v:vb}) = A.foldl (+) zero $ A.zipWith (*) va vb
class TrMat a b | a -> b where
  tr :: a -> b
instance trMatC :: TrMat (Cov a) (Cov a) where
  tr c = c
instance trMatJ :: TrMat (Jac a b) (Jac b a) where
  tr j@(Jac {v: va, nr: r}) = Jac {v: vc, nr: nb} where
    l = A.length va
    na = r
    nb = l `div` na
    vc = fromList $ do
      let ixa = indV nb
      i0 <- range 0 (nb-1)
      j0 <- range 0 (na-1)
      pure $ (uidx va (ixa j0 i0))
class SwMat a b c | a b -> c where
  sw :: a -> b -> c
infixl 7 sw as .*.
instance swVec :: SwMat (Vec a) (Cov a) Number where
  sw v c = v *. (c *. v)
instance swCov :: SwMat (Cov a) (Cov a) (Cov a) where
  sw (Cov {v: va}) (Cov {v: vb}) = Cov {v: v'} where
    l = A.length vb
    n = case l of
              6  -> 3
              10 -> 4
              15 -> 5
              _  -> error $ "sw cov cov: don't know how to " <> show l
    m = n -- > mxn * nxn * nxm -> mxm
    vint :: Array Number
    vint = fromList $ do
      let ixa = indVs n
          ixb = indVs m
          -- ixc = indV m
      i0 <- range 0 (n-1)
      j0 <- range 0 (m-1)
      pure $ sum $ do
        k0 <- range 0 (n-1)
        pure $ (uidx vb (ixa i0 k0)) * (uidx va (ixb k0 j0))
    v' = fromList $ do
      let ixa = indVs m
          ixb = indV m
          -- ixc = indVs m
      i0 <- range 0 (m-1)
      j0 <- range i0 (m-1)
      pure $ sum $ do
        k0 <- range 0 (n-1)
        pure $ (uidx va (ixa k0 i0 )) * (uidx vint (ixb k0 j0))
instance swJac :: SwMat (Jac a b) (Cov a) (Cov b) where
  sw j@(Jac {v: va}) c@(Cov {v: vb}) = Cov {v: v'} where
    l = A.length vb
    n = case l of
              6  -> 3
              10 -> 4
              15 -> 5
              _  -> error $ "swJac: don't know how to " <> show l
    m = (A.length va) `div` n -- > mxn * nxn * nxm -> mxm
    vint :: Array Number
    vint = fromList $ do
      let ixa = indVs n
          ixb = indV m
      i0 <- range 0 (n-1)
      j0 <- range 0 (m-1)
      pure $ sum $ do
        k0 <- range 0 (n-1)
        pure $ (uidx vb (ixa i0 k0)) * (uidx va (ixb k0 j0))
    v' = fromList $ do
      let ixa = indV m
          ixb = indV m
      i0 <- range 0 (m-1)
      j0 <- range i0 (m-1)
      pure $ sum $ do
        k0 <- range 0 (n-1)
        pure $ (uidx va (ixa k0 i0)) * (uidx vint (ixb k0 j0))

instance showCov :: Show (Cov a) where
  show c = "Show (Cov a) \n" <> showMatrix c
instance showVec :: Show (Vec a) where
  show c = "Show (Vec a) \n" <> showMatrix c
instance showJac :: Show (Jac a b) where
  show c = "Show (Jac a b) \n" <> showMatrix c

instance semiringCov3 :: Semiring (Cov Dim3) where
  add (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (+) v1 v2}
  zero = Cov {v: A.replicate 6 0.0 }
  mul (Cov {v: a}) (Cov {v: b}) = error "------------> mul cov3 * cov3 not allowed"
  one = Cov { v: [1.0, 0.0, 0.0, 1.0, 0.0, 1.0] }
instance ringCov3 :: Ring (Cov Dim3) where
  sub (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (-) v1 v2}
instance semiringCov4 :: Semiring (Cov Dim4) where
  add (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (+) v1 v2}
  zero = Cov {v: A.replicate 10 0.0 }
  mul (Cov {v: a}) (Cov {v: b}) = error "------------> mul cov4 * cov4 not allowed"
  one = Cov { v: [1.0,0.0,0.0,0.0,1.0,0.0,0.0,1.0,0.0,1.0] }
instance ringCov4 :: Ring (Cov Dim4) where
  sub (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (-) v1 v2}
instance semiringCov5 :: Semiring (Cov Dim5) where
  add (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (+) v1 v2}
  zero = Cov {v: A.replicate 15 0.0 }
  mul a b = error "------------> mul cov5 * cov5 not allowed"
  one = Cov { v: [1.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0,0.0,0.0,1.0,0.0,1.0] }
instance ringCov5 :: Ring (Cov Dim5) where
  sub (Cov {v: v1}) (Cov {v: v2}) = Cov {v: A.zipWith (-) v1 v2}

instance semiringJac :: Semiring (Jac a b) where
  add (Jac {v: v1}) (Jac {v: v2, nr: r}) = Jac {v: A.zipWith (+) v1 v2, nr: r}
  zero = undefined
  mul (Jac {v: v1}) (Jac {v: v2, nr :r}) = undefined
  one = undefined
instance ringJac :: Ring (Jac a b) where
  sub (Jac {v: v1}) (Jac {v: v2, nr: r}) = Jac {v: A.zipWith (-) v1 v2, nr: r}

-- Instances for Vec -- these are always column vectors
instance semiringVec3 :: Semiring (Vec Dim3) where
  add (Vec {v: v1}) (Vec {v: v2}) = Vec {v: A.zipWith (+) v1 v2}
  zero = Vec {v: A.replicate 3 0.0 }
  mul (Vec {v: v1}) (Vec {v: v2}) = undefined
  one = Vec { v: A.replicate 3 1.0 }
instance ringVec3 :: Ring (Vec Dim3) where
  sub (Vec {v: v1}) (Vec {v: v2}) = Vec {v: A.zipWith (-) v1 v2}
instance semiringVec4 :: Semiring (Vec Dim4) where
  add (Vec {v: v1}) (Vec {v: v2}) = Vec {v: A.zipWith (+) v1 v2}
  zero = Vec {v: A.replicate 4 0.0 }
  mul (Vec {v: v1}) (Vec {v: v2}) = undefined
  one = Vec { v: A.replicate 4 1.0 }
instance ringVec4 :: Ring (Vec Dim4) where
  sub (Vec {v: v1}) (Vec {v: v2}) = Vec {v: A.zipWith (-) v1 v2}
instance semiringVec5 :: Semiring (Vec Dim5) where
  add (Vec {v: v1}) (Vec {v: v2}) = Vec {v: A.zipWith (+) v1 v2}
  zero = Vec {v: A.replicate 5 0.0 }
  mul (Vec {v: v1}) (Vec {v: v2}) = undefined
  one = Vec { v: A.replicate 5 1.0 }
instance ringVec5 :: Ring (Vec Dim5) where
  sub (Vec {v: v1}) (Vec {v: v2}) = Vec {v: A.zipWith (-) v1 v2}

scaleDiag :: Number -> Cov3 -> Cov3
scaleDiag s (Cov {v: v}) = (Cov {v: _sc v}) where
  _sc :: Array Number -> Array Number
  _sc = unsafePartial $ \[a,_,_,b,_,c] -> [s*a,0.0,0.0,s*b,0.0,s*c]

subm :: Int -> Vec5 -> Vec3
subm n (Vec {v: v}) = Vec {v: _subm v} where
  _subm :: Array Number -> Array Number
  _subm = unsafePartial $ \[a,b,c,_,_] -> [a,b,c]

subm2 :: Int -> Cov5 -> Cov3
subm2 n (Cov {v: v}) = Cov {v: _subm2 v} where
  _subm2 :: Array Number -> Array Number
  _subm2 = unsafePartial $ \[a,b,c,_,_,d,e,_,_,f,_,_,_,_,_] -> [a,b,c,d,e,f]

chol :: forall a. Cov a -> Jac a a
chol = choldc

choldc :: forall a. Cov a -> Jac a a
choldc (Cov {v: a}) = Jac {v: a', nr: n} where
  n = case A.length a of
        6  -> 3
        10 -> 4
        15 -> 5
        _  -> 0 -- error $ "choldc: cannot deal with A.length " <> show (A.length a)
  a' = doCholdc a n

cholInv :: forall a. Cov a -> Cov a
cholInv (Cov {v: a}) = Cov {v: a'} where
  n = case A.length a of
        6  -> 3
        10 -> 4
        15 -> 5
        _  -> 0 -- error $ "cholInv: not supported for length " <> show (A.length a)
  a' = doCholInv a n

testCov2 :: String
testCov2 = s where
  xc3 :: Cov Dim3
  xc3 = Cov {v: [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]}
  xj3 :: Jac Dim3 Dim3
  xj3 = Jac {v: [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0], nr: 3}
  xj31 :: Jac Dim3 Dim3
  xj31 = Jac {v: [1.0,0.0,0.0,1.0,1.0,0.0,1.0,1.0,1.0], nr: 3}
  xj32 :: Jac Dim3 Dim3
  xj32 = Jac {v: [0.0,0.0,1.0,0.0,1.0,0.0,1.0,0.0,0.0], nr: 3}
  xj33 :: Jac Dim3 Dim3
  xj33 = Jac {v: [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0], nr: 3}
  xj53 :: Jac Dim5 Dim3
  xj53 = Jac {v: [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0], nr: 5}
  xvc3 = toArray xc3
  xv3 = fromArray [1.0,1.0,1.0] :: Vec3
  xv5 = fromArray [1.0,1.0,1.0,1.0,1.0] :: Vec5
  s =  "Test Cov 2----------------------------------------------\n"
    <> "Vec *. Vec = " <> show (v3 *. v3) <> "\n"
    <> "Cov *. Cov = " <> show (((fromArray[1.0,2.0,3.0,4.0,5.0,6.0])::Cov3) *. ((fromArray [0.0,0.0,1.0,1.0,0.0,0.0])::Cov3) *. inv (one::Cov3)) <> "\n"
--    <> "Vec + Vec = " <> show (v5 + v5) <> "\n"
--    <> "chol Cov = " <> show (chol (one::Cov5)) <> "\n"
--    <> "Vec .*. Cov = " <> show (v5 .*. inv (one::Cov5)) <> "\n"
    <> "xc3 :: Cov Dim3 " <> show xc3
    <> show (toArray $ xc3) <> "\n"
    <> "xj53 ---> " <> show xj53
    <> "xj53 *. xc3 ---> " <> show (xj53 *. xc3)
    <> show xvc3 <> "\n"
        {-- <> show md <> "\n" --}
        {-- <> show mm3 --}
        {-- <> show mm5 --}
        {-- <> "exp v3 " <> show ( (v3 + v3) |.| v3 ) <> "\n" --}
        {-- <> show (j53) --}
        {-- <> show (tr j53) --}
        {-- <> "tj3 " <> show tj3 --}
        {-- <> "vv3 " <> show vv3 --}
        {-- <> show (v3 |*| c3) --}
        {-- <> "\n(tr j53 .*. c3)" <> show (tr j53 .*. c3) --}
        {-- <> "(tr j53 ||| v5)" <> show (tr j53 ||| v5) --}
        {-- <> show (c3 ** (inv c3)) --}
        {-- <> show (c4 ** (inv c4)) --}
    <> "chol: -----------------\n"
    <> "A = L * L^T         " <> show ch3
    <> "L                   " <> show (choldc ch3)
    <> "L * L^T             " <> show ((choldc ch3) *. tr (choldc ch3))
    <> "A^(-1) = L' * L'^T  " <> show (cholInv ch3)
    <> "A * A^(-1)          " <> show (ch3 *. cholInv ch3)
    <> "A = L * L^T         " <> show ch5
    <> "L                   " <> show (choldc ch5)
    <> "L * L^T             " <> show ((choldc ch5) *. tr (choldc ch5))
    <> "A^(-1) = L' * L'^T  " <> show (cholInv ch5)
    <> "A * A^(-1)          " <> show (ch5 *. cholInv ch5)
    <> "det this            " <> show (det ch5)
    <> "\n" -- <> testCov2
  c3 :: Cov3
  c3 = fromArray [1.0,2.0,3.0,4.0,5.0,6.0]
  c4 :: Cov4
  c4 = fromArray [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0]
  c5 :: Cov5
  c5 = fromArray [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0]
  c50 :: Cov5
  c50 = fromArray [15.0,14.0,13.0,12.0,11.0,10.0,9.0,8.0,7.0,6.0,5.0,4.0,3.0,2.0,1.0]
  c51 :: Cov5
  c51 = one
  v3 :: Vec3
  v3 = fromArray [10.0,11.0,12.0]
  v5 :: Vec5
  v5 = fromArray [10.0,11.0,12.0,13.0,14.0]
  j53 :: Jac53
  j53 = Jac { v: [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0], nr: 5}
--  tj3 :: Cov3
--  tj3 = j53 .*. c5
--  vv5 :: Vec5
--  vv5 = j53 *. v3
--  vv3 :: Vec3
--  vv3 = tr j53 *. j53 *. c3 *. v3

  ch3 :: Cov3
  ch3 = fromArray [2.0, -1.0, 0.0, 2.0, -1.0, 2.0]
  cch3 = choldc ch3
  ich3 = cholInv ch3

  ch5 :: Cov5
  ch5 = fromArray [2.0, -1.0, 0.0, 0.0, 0.0, 2.0, -1.0, 0.0, 0.0, 2.0, 0.0, 0.0, 2.0, 0.0, 2.0]
