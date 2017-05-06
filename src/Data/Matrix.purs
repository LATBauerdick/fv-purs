module Data.Matrix (
    Matrix (..)
  , M (..), sw, tr
  , fromList, fromList2, fromLists
  , identity, matrix, diagonal, zero_
  , toList, toLists, getElem
  , nrows, ncols, values
  , getDiag, subm, subm2, scalar
  , (+.), (-.), elementwiseUnsafePlus, elementwiseUnsafeMinus
  , inv, invMaybe, det
  , scaleDiag
  , multStd
  , prettyMatrix
  ) where

import Prelude
import Stuff
import Data.Array (
                    range, index, unsafeIndex, cons, concat, fromFoldable, take
                  , mapMaybe, replicate, slice, length , all, singleton
                  ) as A
import Data.Tuple (Tuple (..), fst, snd)
import Data.List ( List(..), (:), length, head, range ) as L
import Data.Maybe ( Maybe(..), fromJust, fromMaybe )
import Data.Monoid ( class Monoid, mempty )
import Data.Foldable ( foldr, maximum, sum )
import Data.String ( length ) as S
import Data.String.Utils ( fromCharArray ) as S
import Partial.Unsafe ( unsafePartial )

newtype M0 = M0 (Matrix Number)
instance showM0 :: Show M0 where
  show (M0 m) = prettyMatrix m

type M = Matrix Number
-- | sandwich a matrix: a^T * b * a
sw :: M -> M -> M
sw a b = (tr a) * b * a
{-- sw a b = (tr a) * b * a --}
tr :: M -> M
tr = transpose
-- vectors are column-wise, represented as matrix of dimension nx1
subm :: Int -> M -> M
subm n v = submatrix 1 n 1 1 v
subm2 :: Int -> M -> M
subm2 n m = submatrix 1 n 1 n m

scaleDiag :: Number -> M -> M
scaleDiag s = (diagonal 0.0 <<< getDiag <<< scaleMatrix s)

scalar :: M -> Number
scalar m = getElem 1 1 m

{-- fromList :: Int -> Array Number -> M0 --}
{-- fromList r ds = M0 (unsafePartial $ fromJust $ fromArray r 1 ds) --}
{-- fromList2 :: Int -> Int -> Array Number -> M0 --}
{-- fromList2 r c ds = M0 (unsafePartial $ fromJust $ fromArray r c ds) --}

det :: M -> Number
det m = 1.0-- M.detLU m

invMaybe :: M -> Maybe M
invMaybe m = Just m
{-- invMaybe m = case invsm m of --}
{--                Right im -> Just im --}
{--                Left s -> Nothing `debug` ("Error in Matrix.invsm: " ++ s) --}

inv :: M -> M
inv m = m
{-- inv m =  let (Right m') = do { invm m } `catchError` printError --}
{--           in m' --}
{--          where --}
{--            one = (M.identity $ M.nrows m) --}
{--            printError :: InvError -> InvMonad M --}
{--            printError e = return one `debug` ("Error in Matrix.inv: " ++ (show (quality e)) ++ ": " ++ (reason e)) --}

-- | Dense Vector implementation
type Vector = Array

-- | Dense Matrix implementation
-- | Type of matrices.
--
--   Elements can be of any type. Rows and columns
--   are indexed starting by 1. This means that, if @m :: Matrix a@ and
--   @i,j :: Int@, then @m ! (i,j)@ is the element in the @i@-th row and
--   @j@-th column of @m@.
data Matrix a = M_
  { nrows :: Int
  , ncols :: Int
  , values :: Array a
  }
nrows :: forall a. Matrix a -> Int
nrows (M_ {nrows: r}) = r
ncols :: forall a. Matrix a -> Int
ncols (M_ {ncols: c}) = c
values :: forall a. Matrix a -> Array a
values (M_ m) = m.values

encode :: Int -> Int -> Int -> Int
encode m i j = (i-1)*m + j - 1

decode :: Int -> Int -> (Tuple Int Int)
decode m k = (Tuple (q+1) (r+1))
-- where (Tuple q r) = quotRem k m
  where qr = quotRem k m
        q = fst qr
        r = snd qr

instance eqMatrix :: Eq a => Eq (Matrix a) where
  eq m1 m2 | nrows m1 /= nrows m2 || ncols m1 /= ncols m2 = false
           | otherwise = A.all id pa where
              pa = do
                i <- A.range 1 (nrows m1)
                j <- A.range 1 (ncols m1)
                pure $ getElem i j m1 == getElem i j m2

-- | Just a cool way to output the size of a matrix.
sizeStr :: Int -> Int -> String
sizeStr n m = show n <> "x" <> show m

-- | Display a matrix as a 'String' using the 'Show' instance of its elements.
prettyMatrix :: forall a. Show a => Matrix a -> String
--prettyMatrix (M_ m) = show m.values
prettyMatrix m@(M_ {nrows: r, ncols: c, values: v}) = unlines ls where
  ls = do
    i <- L.range 1 r
    let ws :: L.List String
        ws = map (\j -> fillBlanks mx (show $ getElem i j m)) (L.range 1 c)
    pure $ "( " <> unwords ws <> " )"
  mx = fromMaybe 0 (maximum $ map (S.length <<< show) v)
  fillBlanks k str = (S.fromCharArray $ A.replicate (k - S.length str) " ") <> str

instance showMatrix :: Show a => Show (Matrix a) where
  show = prettyMatrix

-- | /O(rows*cols)/. Similar to 'V.force'. It copies the matrix content
--   dropping any extra memory.
--
--   Useful when using 'submatrix' from a big matrix.
--
forceMatrix :: forall a. Matrix a -> Matrix a
forceMatrix m = matrix (nrows m) (ncols m) $ \(Tuple i j) -> unsafeGet i j m
-------------------------------------------------------
-------------------------------------------------------
---- FUNCTOR INSTANCE

instance functorMatrix :: Functor Matrix where
  map f (M_ {nrows: r, ncols: c, values: v}) = M_ {nrows: r, ncols: c, values: map f v}

-------------------------------------------------------
-------------------------------------------------------

-------------------------------------------------------
-------------------------------------------------------
---- MONOID INSTANCE

-- instance monoidMatrix :: Monoid a => Monoid (Matrix a) where
--   mempty = fromList 1 1 [mempty]
--  append m m' = matrix (max (nrows m) (nrows m')) (max (ncols m) (ncols m')) $ uncurry zipTogether --}
--     where zipTogether row column = fromMaybe mempty $ safeGet row column m <> safeGet row column m'

-- | /O(rows*cols)/. The transpose of a matrix.
--   Example:
--
-- >           ( 1 2 3 )   ( 1 4 7 )
-- >           ( 4 5 6 )   ( 2 5 8 )
-- > transpose ( 7 8 9 ) = ( 3 6 9 )
{-- transpose :: Matrix a -> Matrix a --}
{-- transpose m = matrix (ncols m) (nrows m) $ \(i,j) -> m ! (j,i) --}

-- | /O(rows*cols)/. Map a function over a row.
--   Example:
--
-- >                          ( 1 2 3 )   ( 1 2 3 )
-- >                          ( 4 5 6 )   ( 5 6 7 )
-- > mapRow (\_ x -> x + 1) 2 ( 7 8 9 ) = ( 7 8 9 )
--
{-- mapRow :: (Int -> a -> a) -- ^ Function takes the current column as additional argument. --}
{--         -> Int            -- ^ Row to map. --}
{--         -> Matrix a -> Matrix a --}
{-- mapRow f r m = --}
{--   matrix (nrows m) (ncols m) $ \(i,j) -> --}
{--     let a = unsafeGet i j m --}
{--     in  if i == r --}
{--            then f j a --}
{--            else a --}

-- | /O(rows*cols)/. Map a function over a column.
--   Example:
--
-- >                          ( 1 2 3 )   ( 1 3 3 )
-- >                          ( 4 5 6 )   ( 4 6 6 )
-- > mapCol (\_ x -> x + 1) 2 ( 7 8 9 ) = ( 7 9 9 )
--
{-- mapCol :: (Int -> a -> a) -- ^ Function takes the current row as additional argument. --}
{--         -> Int            -- ^ Column to map. --}
{--         -> Matrix a -> Matrix a --}
{-- mapCol f c m = --}
{--   matrix (nrows m) (ncols m) $ \(i,j) -> --}
{--     let a = unsafeGet i j m --}
{--     in  if j == c --}
{--            then f i a --}
{--            else a --}

-------------------------------------------------------
-------------------------------------------------------
---- FOLDABLE AND TRAVERSABLE INSTANCES

{-- instance foldableMatrix :: Foldable Matrix where --}
{--   foldMap f = foldMap f <<< values <<< forceMatrix --}

{-- instance Traversable Matrix where --}
{--  sequenceA m = fmap (M (nrows m) (ncols m) 0 0 (ncols m)) . sequenceA . mvect $ forceMatrix m --}

-------------------------------------------------------
-------------------------------------------------------
---- BUILDERS

-- | /O(rows*cols)/. The zero matrix of the given size.
--
-- > zero n m =
-- >                 m
-- >   1 ( 0 0 ... 0 0 )
-- >   2 ( 0 0 ... 0 0 )
-- >     (     ...     )
-- >     ( 0 0 ... 0 0 )
-- >   n ( 0 0 ... 0 0 )
zero_ ::
     Int -- ^ Rows
  -> Int -- ^ Columns
  -> Matrix Number
zero_ n m = M_ { nrows: n, ncols: m, values: (A.replicate (n*m) 0.0)}
zeroInt_ ::
     Int -- ^ Rows
  -> Int -- ^ Columns
  -> Matrix Int
zeroInt_ n m = M_ { nrows: n, ncols: m, values: (A.replicate (n*m) 0)}

-- | /O(rows*cols)/. Generate a matrix from a generator function.
--   Example of usage:
--
-- >                                  (  1  0 -1 -2 )
-- >                                  (  3  2  1  0 )
-- >                                  (  5  4  3  2 )
-- > matrix 4 4 $ \(i,j) -> 2*i - j = (  7  6  5  4 )
matrix :: forall a. Int -- ^ Rows
       -> Int -- ^ Columns
       -> ((Tuple Int Int) -> a) -- ^ Generator function
       -> Matrix a
matrix n m f = M_ {nrows: n, ncols: m, values: val} where
--  val = undefined
  val = do
    i <- A.range 1 n
    j <- A.range 1 m
    pure $ f (Tuple i j)

-- | /O(rows*cols)/. Identity matrix of the given order.
--
-- > identity n =
-- >                 n
-- >   1 ( 1 0 ... 0 0 )
-- >   2 ( 0 1 ... 0 0 )
-- >     (     ...     )
-- >     ( 0 0 ... 1 0 )
-- >   n ( 0 0 ... 0 1 )
--
identity :: Int -> Matrix Number
identity n = M_ {nrows: n, ncols: n, values: val} where
  val = do
    i <- A.range 0 (n-1)
    j <- A.range 0 (n-1)
    pure $ if i==j then 1.00 else 0.0

identityInt :: Int -> Matrix Int
identityInt n = M_ {nrows: n, ncols: n, values: val} where
  val = do
    i <- A.range 0 (n-1)
    j <- A.range 0 (n-1)
    pure $ if i==j then 1 else 0

-- | Similar to 'diagonalList' but with A.Array, which
--   should be more efficient.
diagonal :: forall a. a -- ^ Default element
         -> Array a  -- ^ Diagonal vector
         -> Matrix a
diagonal e v = matrix n n $ \(Tuple i j) -> if i == j
                                               then unsafePartial $ A.unsafeIndex v (i - 1)
                                               else e
  where
    n = A.length v

-- | Create a matrix from a non-empty list given the desired size.
--   The list must have at least /rows*cols/ elements.
--   An example:
--
-- >                           ( 1 2 3 )
-- >                           ( 4 5 6 )
-- > fromList2 3 3 (1 .. 9) =  ( 7 8 9 )
--
-- | Create column vector from array
fromList :: forall a. Int -> Array a -> Matrix a
fromList r vs = M_ {nrows: r, ncols: 1, values: vs}
-- | Create matrix from array
fromList2 :: forall a. Int -> Int -> Array a -> Matrix a
fromList2 r c vs = M_ {nrows: r, ncols: c, values: vs}

-- | Get the elements of a matrix stored in a list.
--
-- >        ( 1 2 3 )
-- >        ( 4 5 6 )
-- > toList ( 7 8 9 ) = [1,2,3,4,5,6,7,8,9]
--
toList :: forall a. Matrix a -> Array a
toList m = values m
  {-- do --}
  {--   i <- A.range 1 (nrows m) --}
  {--   j <- A.range 1 (ncols m) --}
  {--   pure $ unsafeGet i j m --}

-- | Get the elements of a matrix stored in a list of lists,
--   where each list contains the elements of a single row.
--
-- >         ( 1 2 3 )   [ [1,2,3]
-- >         ( 4 5 6 )   , [4,5,6]
-- > toLists ( 7 8 9 ) = , [7,8,9] ]
--
toLists :: forall a. Matrix a -> Array (Array a)
toLists m = do
  j <- A.range 1 (ncols m)
  pure $ do
    i <- A.range 1 (nrows m)
    pure $ unsafeGet i j m

-- | Diagonal matrix from a non-empty list given the desired size.
--   Non-diagonal elements will be filled with the given default element.
--   The list must have at least /order/ elements.
--
-- > diagonalList n 0 [1..] =
-- >                   n
-- >   1 ( 1 0 ... 0   0 )
-- >   2 ( 0 2 ... 0   0 )
-- >     (     ...       )
-- >     ( 0 0 ... n-1 0 )
-- >   n ( 0 0 ... 0   n )
--
diagonalList :: forall a. Int -> a -> Array a -> Matrix a
diagonalList n e xs = matrix n n $ \(Tuple i j) -> if i == j 
                                                      then unsafePartial $ A. unsafeIndex xs (i - 1)
                                                      else e

fromLists :: forall a. L.List (Array a) -> Matrix a
fromLists L.Nil = error "fromLists: empty list."
fromLists xss = fromList2 n m $ foldr (<>) [] xss
  where
    n = L.length xss
    m = A.length $ unsafePartial $ fromJust $ L.head xss

-- | /O(1)/. Represent a vector as a one row matrix.
rowVector :: forall a. Array a -> Matrix a
rowVector v = M_ {nrows: 1, ncols: A.length v, values: v}

-- | /O(1)/. Represent a vector as a one column matrix.
colVector :: forall a. Array a -> Matrix a
colVector v = M_ { nrows: (A.length v), ncols: 1, values: v }

-- | /O(rows*cols)/. Permutation matrix.
--
-- > permMatrix n i j =
-- >               i     j       n
-- >   1 ( 1 0 ... 0 ... 0 ... 0 0 )
-- >   2 ( 0 1 ... 0 ... 0 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >   i ( 0 0 ... 0 ... 1 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >   j ( 0 0 ... 1 ... 0 ... 0 0 )
-- >     (     ...   ...   ...     )
-- >     ( 0 0 ... 0 ... 0 ... 1 0 )
-- >   n ( 0 0 ... 0 ... 0 ... 0 1 )
--
-- When @i == j@ it reduces to 'identity' @n@.
--
permMatrix :: forall a. Int -- ^ Size of the matrix.
           -> Int -- ^ Permuted row 1.
           -> Int -- ^ Permuted row 2.
           -> Matrix a -- ^ Permutation matrix.
permMatrix n r1 r2 = undefined
{-- permMatrix n r1 r2 | r1 == r2 = identity n --}
{-- permMatrix n r1 r2 = matrix n n f --}
{--  where --}
{--   f (i,j) --}
{--    | i == r1 = if j == r2 then 1 else 0 --}
{--    | i == r2 = if j == r1 then 1 else 0 --}
{--    | i == j = 1 --}
{--    | otherwise = 0 --}

-------------------------------------------------------
-------------------------------------------------------
---- ACCESSING

-- | /O(1)/. Get an element of a matrix. Indices range from /(1,1)/ to /(n,m)/.
--   It returns an 'error' if the requested element is outside of range.

getElem :: ∀ a.
           Int      -- ^ Row
        -> Int      -- ^ Column
        -> Matrix a -- ^ Matrix
        -> a
getElem i j m =
  case safeGet i j m of
    Just x -> x
    Nothing -> error
      $ "getElem: Trying to get the ("
      <> show i <> ", " <> show j
      <> ") element from a "
      <> sizeStr (nrows m) (ncols m)
      <> " matrix."

-- | Variant of 'getElem' that returns Maybe instead of an error.
safeGet :: forall a. Int -> Int -> Matrix a -> Maybe a
safeGet i j a@(M_ {nrows: r, ncols: c, values: v})
 | i > r || j > c || r < 1 || c < 1 = Nothing
 | otherwise = Just $ unsafeGet i j a

-- | /O(1)/. Unsafe variant of 'getElem', without bounds checking.
unsafeGet :: forall a.
             Int      -- ^ Row
             -> Int      -- ^ Column
             -> Matrix a -- ^ Matrix
             -> a

unsafeGet i j (M_ {ncols: c, values: v}) = unsafePartial $ A.unsafeIndex v $ encode w (i+ro) (j+co)
  where w = c
        ro = 0
        co = 0

{-- -- | Short alias for 'getElem'. --}
{-- getElem_ :: forall a. Matrix a -> Array Int -> a --}
{-- getElem_ m [i,j] = getElem i j m --}
{-- infixl 8 getElem_ as ! --}

{-- -- | Internal alias for 'unsafeGet'. --}
{-- unsafeGet_ :: forall a. Matrix a -> Array Int -> a --}
{-- unsafeGet_ m [i,j] = unsafeGet i j m --}
{-- infixl 8 unsafeGet_ as !. --}


--
----
----
-----
--


-- | /O(rows*cols)/. The transpose of a matrix.
--   Example:
--
-- >           ( 1 2 3 )   ( 1 4 7 )
-- >           ( 4 5 6 )   ( 2 5 8 )
-- > transpose ( 7 8 9 ) = ( 3 6 9 )
transpose :: forall a. Matrix a -> Matrix a
transpose m = matrix (ncols m) (nrows m) $ \(Tuple i j) -> getElem  j i m

--
----
-----
----
----
--
--

-------------------------------------------------------
-------------------------------------------------------
---- MATRIX OPERATIONS

-- | Perform an operation element-wise.
--   The second matrix must have at least as many rows
--   and columns as the first matrix. If it's bigger,
--   the leftover items will be ignored.
--   If it's smaller, it will cause a run-time error.
--   You may want to use 'elementwiseUnsafe' if you
--   are definitely sure that a run-time error won't
--   arise.
elementwise :: forall a b c. (a -> b -> c) -> (Matrix a -> Matrix b -> Matrix c)
elementwise f m m' = matrix (nrows m) (ncols m) $
  \(Tuple i j) -> f (getElem i j m) (getElem i j m')

-- | Unsafe version of 'elementwise', but faster.
elementwiseUnsafe :: forall a b c. (a -> b -> c) -> (Matrix a -> Matrix b -> Matrix c)
{-# INLINE elementwiseUnsafe #-}
elementwiseUnsafe f m m' = matrix (nrows m) (ncols m) $
  \(Tuple i j) -> f (unsafeGet i j m) (unsafeGet i j m')

-- | Internal unsafe addition.
elementwiseUnsafePlus :: forall a. Ring a => Matrix a -> Matrix a -> Matrix a
elementwiseUnsafePlus a b = elementwiseUnsafe (+) a b
infixl 6 elementwiseUnsafePlus as +.
-- | Internal unsafe substraction.
elementwiseUnsafeMinus :: forall a. Ring a => Matrix a -> Matrix a -> Matrix a
elementwiseUnsafeMinus a b = elementwiseUnsafe (-) a b
infixl 6 elementwiseUnsafeMinus as -.

-------------------------------------------------------
-------------------------------------------------------
--
instance semiringMatrixiNumber ∷ Semiring (Matrix Number) where
  add  = elementwiseUnsafePlus
  mul  = multStd
  zero = zero_ 3 3
  one  = identity 3

instance semiringMatrixInt ∷ Semiring (Matrix Int) where
  add  = elementwiseUnsafePlus
  mul  = multStd
  zero = zeroInt_ 3 3
  one  = identityInt 3

instance ringMatrixNumber ∷ Ring (Matrix Number) where
  sub = elementwiseUnsafeMinus
instance ringMatrixInt ∷ Ring (Matrix Int) where
  sub = elementwiseUnsafeMinus

--
-------------------------------------------------------
-------------------------------------------------------
---- MATRIX MULTIPLICATION

{- $mult

Four methods are provided for matrix multiplication.

* 'multStd':
     Matrix multiplication following directly the definition.
     This is the best choice when you know for sure that your
     matrices are small.

* 'multStd2':
     Matrix multiplication following directly the definition.
     However, using a different definition from 'multStd'.
     According to our benchmarks with this version, 'multStd2' is
     around 3 times faster than 'multStd'.

* 'multStrassen':
     Matrix multiplication following the Strassen's algorithm.
     Complexity grows slower but also some work is added
     partitioning the matrix. Also, it only works on square
     matrices of order @2^n@, so if this condition is not
     met, it is zero-padded until this is accomplished.
     Therefore, its use is not recommended.

* 'multStrassenMixed':
     This function mixes the previous methods.
     It provides a better performance in general. Method @(@'*'@)@
     of the 'Num' class uses this function because it gives the best
     average performance. However, if you know for sure that your matrices are
     small (size less than 500x500), you should use 'multStd' or 'multStd2' instead,
     since 'multStrassenMixed' is going to switch to those functions anyway.

We keep researching how to get better performance for matrix multiplication.
If you want to be on the safe side, use ('*').

-}

infixl 6 multStd as *.

-- | Standard matrix multiplication by definition.
multStd :: forall a. Semiring a => Matrix a -> Matrix a -> Matrix a
multStd a1@(M_ {nrows: n, ncols: m}) a2@(M_ {nrows: n', ncols: m'})
   | m /= n' = error $ "Matrix multiplication of " <> sizeStr n m <> " and "
                    <> sizeStr n' m' <> " matrices."
   | otherwise = multStd_ a1 a2

-- | Standard matrix multiplication by definition.
multStd2 :: forall a. Semiring a => Matrix a -> Matrix a -> Matrix a
multStd2 a1@(M_ {nrows: n, ncols: m}) a2@(M_ {nrows: n', ncols: m'})
   | m /= n' = error $ "Matrix multiplication of " <> sizeStr n m <> " and "
                    <> sizeStr n' m' <> " matrices."
   | otherwise = multStd__ a1 a2

-- | Standard matrix multiplication by definition, without checking if sizes match.
multStd_ :: forall a. Semiring a => Matrix a -> Matrix a -> Matrix a
multStd_ a@(M_ {nrows: 1, ncols: 1, values: av}) b@(M_ {nrows: 1, ncols: 1, values: bv}) =
  M_ {nrows: 1, ncols: 1, values: v} where
   v = A.singleton $ (unsafePartial $ A.unsafeIndex av 0) * (unsafePartial $ A.unsafeIndex bv 0)
multStd_ a@(M_ {nrows: 2, ncols: 2, values: av})
         b@(M_ {nrows: 2, ncols:  2, values: bv}) =
  M_ {nrows: 2, ncols: 2, values: v} where
    a11 = unsafePartial $ A.unsafeIndex av 0
    a12 = unsafePartial $ A.unsafeIndex av 1
    a21 = unsafePartial $ A.unsafeIndex av 2
    a22 = unsafePartial $ A.unsafeIndex av 3
    b11 = unsafePartial $ A.unsafeIndex bv 0
    b12 = unsafePartial $ A.unsafeIndex bv 1
    b21 = unsafePartial $ A.unsafeIndex bv 2
    b22 = unsafePartial $ A.unsafeIndex bv 3
    v = [ a11*b11 + a12*b21 , a11*b12 + a12*b22
         , a21*b11 + a22*b21 , a21*b12 + a22*b22
        ]
multStd_ a@(M_ {nrows: 3, ncols: 3, values: av})
         b@(M_ {nrows: 3, ncols: 3, values: bv}) =
  M_ {nrows: 3, ncols: 3, values: v} where
    a11 = unsafePartial $ A.unsafeIndex av 0
    a12 = unsafePartial $ A.unsafeIndex av 1
    a13 = unsafePartial $ A.unsafeIndex av 2
    a21 = unsafePartial $ A.unsafeIndex av 3
    a22 = unsafePartial $ A.unsafeIndex av 4
    a23 = unsafePartial $ A.unsafeIndex av 5
    a31 = unsafePartial $ A.unsafeIndex av 6
    a32 = unsafePartial $ A.unsafeIndex av 7
    a33 = unsafePartial $ A.unsafeIndex av 8
    b11 = unsafePartial $ A.unsafeIndex bv 0
    b12 = unsafePartial $ A.unsafeIndex bv 1
    b13 = unsafePartial $ A.unsafeIndex bv 2
    b21 = unsafePartial $ A.unsafeIndex bv 3
    b22 = unsafePartial $ A.unsafeIndex bv 4
    b23 = unsafePartial $ A.unsafeIndex bv 5
    b31 = unsafePartial $ A.unsafeIndex bv 6
    b32 = unsafePartial $ A.unsafeIndex bv 7
    b33 = unsafePartial $ A.unsafeIndex bv 8
    v = [ a11*b11 + a12*b21 + a13*b31
        , a11*b12 + a12*b22 + a13*b32
        , a11*b13 + a12*b23 + a13*b33
        , a21*b11 + a22*b21 + a23*b31
        , a21*b12 + a22*b22 + a23*b32
        , a21*b13 + a22*b23 + a23*b33
        , a31*b11 + a32*b21 + a33*b31
        , a31*b12 + a32*b22 + a33*b32
        , a31*b13 + a32*b23 + a33*b33
        ]
multStd_ a@(M_ {nrows: n, ncols: m}) b@(M_ {ncols: m'}) =
  matrix n m' \(Tuple i j) -> sum do
                                    k <- A.range 1 m
                                    pure $ (getElem i k a ) * (getElem k j b)
multStd_ a b = undefined

multStd__ :: forall a. Matrix a -> Matrix a -> Matrix a
multStd__ a b = undefined
{-- multStd__ a b = matrix r c $ \(i,j) -> dotProduct (V.unsafeIndex avs $ i - 1) (V.unsafeIndex bvs $ j - 1) --}
{--   where --}
{--     r = nrows a --}
{--     avs = V.generate r $ \i -> getRow (i+1) a --}
{--     c = ncols b --}
{--     bvs = V.generate c $ \i -> getCol (i+1) b --}

{-- dotProduct :: Num a => V.Vector a -> V.Vector a -> a --}
{-- {-# INLINE dotProduct #-} --}
{-- dotProduct v1 v2 = numLoopFold 0 (V.length v1 - 1) 0 $ --}
{--   \r i -> V.unsafeIndex v1 i * V.unsafeIndex v2 i + r --}

{-- {- --}
{-- dotProduct v1 v2 = go (V.length v1 - 1) 0 --}
{--   where --}
{--     go (-1) a = a --}
{--     go i a = go (i-1) $ (V.unsafeIndex v1 i) * (V.unsafeIndex v2 i) + a --}
{-- -} --}

{-- first :: (a -> Bool) -> [a] -> a --}
{-- first f = go --}
{--  where --}
{--   go (x:xs) = if f x then x else go xs --}
{--   go _ = error "first: no element match the condition." --}

{-- -- | Strassen's algorithm over square matrices of order @2^n@. --}
{-- strassen :: Num a => Matrix a -> Matrix a -> Matrix a --}
{-- -- Trivial 1x1 multiplication. --}
{-- strassen a@(M 1 1 _ _ _ _) b@(M 1 1 _ _ _ _) = M 1 1 0 0 1 $ V.singleton $ (a ! (1,1)) * (b ! (1,1)) --}
{-- -- General case guesses that the input matrices are square matrices --}
{-- -- whose order is a power of two. --}
{-- strassen a b = joinBlocks (c11,c12,c21,c22) --}
{--  where --}
{--   -- Size of the subproblem is halved. --}
{--   n = div (nrows a) 2 --}
{--   -- Split of the original problem into smaller subproblems. --}
{--   (a11,a12,a21,a22) = splitBlocks n n a --}
{--   (b11,b12,b21,b22) = splitBlocks n n b --}
{--   -- The seven Strassen's products. --}
{--   p1 = strassen (a11 + a22) (b11 + b22) --}
{--   p2 = strassen (a21 + a22)  b11 --}
{--   p3 = strassen  a11        (b12 - b22) --}
{--   p4 = strassen        a22  (b21 - b11) --}
{--   p5 = strassen (a11 + a12)        b22 --}
{--   p6 = strassen (a21 - a11) (b11 + b12) --}
{--   p7 = strassen (a12 - a22) (b21 + b22) --}
{--   -- Merging blocks --}
{--   c11 = p1 + p4 - p5 + p7 --}
{--   c12 = p3 + p5 --}
{--   c21 = p2 + p4 --}
{--   c22 = p1 - p2 + p3 + p6 --}

{-- -- | Strassen's matrix multiplication. --}
{-- multStrassen :: Num a => Matrix a -> Matrix a -> Matrix a --}
{-- multStrassen a1@(M n m _ _ _ _) a2@(M n' m' _ _ _ _) --}
{--    | m /= n' = error $ "Multiplication of " ++ sizeStr n m ++ " and " --}
{--                     ++ sizeStr n' m' ++ " matrices." --}
{--    | otherwise = --}
{--        let mx = maximum [n,m,n',m'] --}
{--            n2  = first (>= mx) $ fmap (2^) [(0 :: Int)..] --}
{--            b1 = setSize 0 n2 n2 a1 --}
{--            b2 = setSize 0 n2 n2 a2 --}
{--        in  submatrix 1 n 1 m' $ strassen b1 b2 --}

{-- strmixFactor :: Int --}
{-- strmixFactor = 300 --}

{-- -- | Strassen's mixed algorithm. --}
{-- strassenMixed :: Num a => Matrix a -> Matrix a -> Matrix a --}
{-- {-# SPECIALIZE strassenMixed :: Matrix Double -> Matrix Double -> Matrix Double #-} --}
{-- {-# SPECIALIZE strassenMixed :: Matrix Int -> Matrix Int -> Matrix Int #-} --}
{-- {-# SPECIALIZE strassenMixed :: Matrix Rational -> Matrix Rational -> Matrix Rational #-} --}
{-- strassenMixed a b --}
{--  | r < strmixFactor = multStd__ a b --}
{--  | odd r = let r' = r + 1 --}
{--                a' = setSize 0 r' r' a --}
{--                b' = setSize 0 r' r' b --}
{--            in  submatrix 1 r 1 r $ strassenMixed a' b' --}
{--  | otherwise = --}
{--       M r r 0 0 r $ V.create $ do --}
{--          v <- MV.unsafeNew (r*r) --}
{--          let en = encode r --}
{--              n' = n + 1 --}
{--          -- c11 = p1 + p4 - p5 + p7 --}
{--          sequence_ [ MV.write v k $ --}
{--                          unsafeGet i j p1 --}
{--                        + unsafeGet i j p4 --}
{--                        - unsafeGet i j p5 --}
{--                        + unsafeGet i j p7 --}
{--                    | i <- [1..n] --}
{--                    , j <- [1..n] --}
{--                    , let k = en (i,j) --}
{--                      ] --}
{--          -- c12 = p3 + p5 --}
{--          sequence_ [ MV.write v k $ --}
{--                          unsafeGet i j' p3 --}
{--                        + unsafeGet i j' p5 --}
{--                    | i <- [1..n] --}
{--                    , j <- [n'..r] --}
{--                    , let k = en (i,j) --}
{--                    , let j' = j - n --}
{--                      ] --}
{--          -- c21 = p2 + p4 --}
{--          sequence_ [ MV.write v k $ --}
{--                          unsafeGet i' j p2 --}
{--                        + unsafeGet i' j p4 --}
{--                    | i <- [n'..r] --}
{--                    , j <- [1..n] --}
{--                    , let k = en (i,j) --}
{--                    , let i' = i - n --}
{--                      ] --}
{--          -- c22 = p1 - p2 + p3 + p6 --}
{--          sequence_ [ MV.write v k $ --}
{--                          unsafeGet i' j' p1 --}
{--                        - unsafeGet i' j' p2 --}
{--                        + unsafeGet i' j' p3 --}
{--                        + unsafeGet i' j' p6 --}
{--                    | i <- [n'..r] --}
{--                    , j <- [n'..r] --}
{--                    , let k = en (i,j) --}
{--                    , let i' = i - n --}
{--                    , let j' = j - n --}
{--                      ] --}
{--          return v --}
{--  where --}
{--   r = nrows a --}
{--   -- Size of the subproblem is halved. --}
{--   n = quot r 2 --}
{--   -- Split of the original problem into smaller subproblems. --}
{--   (a11,a12,a21,a22) = splitBlocks n n a --}
{--   (b11,b12,b21,b22) = splitBlocks n n b --}
{--   -- The seven Strassen's products. --}
{--   p1 = strassenMixed (a11 +. a22) (b11 +. b22) --}
{--   p2 = strassenMixed (a21 +. a22)  b11 --}
{--   p3 = strassenMixed  a11         (b12 -. b22) --}
{--   p4 = strassenMixed         a22  (b21 -. b11) --}
{--   p5 = strassenMixed (a11 +. a12)         b22 --}
{--   p6 = strassenMixed (a21 -. a11) (b11 +. b12) --}
{--   p7 = strassenMixed (a12 -. a22) (b21 +. b22) --}

{-- -- | Mixed Strassen's matrix multiplication. --}
{-- multStrassenMixed :: Num a => Matrix a -> Matrix a -> Matrix a --}
{-- {-# INLINE multStrassenMixed #-} --}
{-- multStrassenMixed a1@(M n m _ _ _ _) a2@(M n' m' _ _ _ _) --}
{--    | m /= n' = error $ "Multiplication of " ++ sizeStr n m ++ " and " --}
{--                     ++ sizeStr n' m' ++ " matrices." --}
{--    | n < strmixFactor = multStd__ a1 a2 --}
{--    | otherwise = --}
{--        let mx = maximum [n,m,n',m'] --}
{--            n2 = if even mx then mx else mx+1 --}
{--            b1 = setSize 0 n2 n2 a1 --}
{--            b2 = setSize 0 n2 n2 a2 --}
{--        in  submatrix 1 n 1 m' $ strassenMixed b1 b2 --}

-------------------------------------------------------
-------------------------------------------------------
---- TRANSFORMATIONS

-- | Scale a matrix by a given factor.
--   Example:
--
-- >               ( 1 2 3 )   (  2  4  6 )
-- >               ( 4 5 6 )   (  8 10 12 )
-- > scaleMatrix 2 ( 7 8 9 ) = ( 14 16 18 )
scaleMatrix :: forall a. Ring a => a -> Matrix a -> Matrix a
scaleMatrix = map <<< (*)




-- | /O(min rows cols)/. Diagonal of a /not necessarily square/ matrix.
getDiag :: forall a. Matrix a -> Array a
getDiag a@(M_ {nrows: n, ncols: m}) = v where
  k = min n m
  v = do
    i <- A.range 1 k
    pure $ getElem i i a

-------------------------------------------------------
-------------------------------------------------------
---- WORKING WITH BLOCKS

-- | /O(1)/. Extract a submatrix given row and column limits.
--   Example:
--
-- >                   ( 1 2 3 )
-- >                   ( 4 5 6 )   ( 2 3 )
-- > submatrix 1 2 2 3 ( 7 8 9 ) = ( 5 6 )
submatrix :: forall a. Int    -- ^ Starting row
          -> Int -- ^ Ending row
          -> Int    -- ^ Starting column
          -> Int -- ^ Ending column
          -> Matrix a
          -> Matrix a
submatrix r1 r2 c1 c2 a@(M_ {nrows: n, ncols: m, values: v})
  | r1 < 1  || r1 > n = error $ "submatrix: starting row (" <> show r1 <> ") is out of range. Matrix has " <> show n <> " rows."
  | c1 < 1  || c1 > m = error $ "submatrix: starting column (" <> show c1 <> ") is out of range. Matrix has " <> show m <> " columns."
  | r2 < r1 || r2 > n = error $ "submatrix: ending row (" <> show r2 <> ") is out of range. Matrix has " <> show n <> " rows, and starting row is " <> show r1 <> "."
  | c2 < c1 || c2 > m = error $ "submatrix: ending column (" <> show c2 <> ") is out of range. Matrix has " <> show m <> " columns, and starting column is " <> show c1 <> "."
  | r1 == 1 && c1 == 1 = M_ {nrows: (r2-r1+1), ncols: (c2-c1+1), values: v'} where
    v' = do
      i <- A.range r1 r2
      j <- A.range c1 c2
      pure $ getElem i j a
  | otherwise = error $ "submatrix: cannot do it" --M (r2-r1+1) (c2-c1+1) (ro+r1-1) (co+c1-1) w v

--------------------------------------------------------------------------------

-- | Create array of given dimmension containing replicated value
replicate :: ∀ a. Int -> Int -> a -> Maybe (Matrix a )
replicate r c v | r > 0 && c > 0 = Just $ M_ {nrows: r, ncols: c, values: A.replicate (r * c) v}
                | otherwise = Nothing


-- | Create array of given dimmension with all values set to 0
zeros :: Int -> Int -> Maybe (Matrix Number)
zeros r c = replicate r c 0.0


-- | Create Matrix from Array
fromArray :: ∀ a. Int -> Int -> Array a -> Maybe (Matrix a)
fromArray r c vs | r > 0 && c > 0 && r*c == A.length vs = Just $ M_ {nrows: r, ncols: c, values: vs}
                 | otherwise = Nothing


-- | Get specific column as a vector. Index is 0 based
-- | If the index is out of range then return empty vector
column :: ∀ a. Int -> Matrix a -> Vector a
column c (M_ mat) = A.mapMaybe (\i -> A.index mat.values (i*mat.ncols+c)) (A.range 0 (mat.nrows-1))


-- | Get specific row as a vector. Index is 0 based
-- | If the index is out of range then return empty vector
row :: ∀ a. Int -> Matrix a -> Vector a
row r (M_ mat) = A.slice i j mat.values
  where
    i = if r >=0 && r < mat.nrows then r*mat.ncols else 0
    j = if r >=0 && r < mat.nrows then i+mat.ncols else 0


-- | Get specific element. Index is 0 based
element :: ∀ a. Int -> Int -> Matrix a -> Maybe a
element r c (M_ mat) = A.index mat.values ((r*mat.ncols) + c)


-- | Return list of rows
rows :: ∀ a. Matrix a -> Array (Vector a)
rows (M_ mat) = do 
  i <- A.range 0 (mat.nrows - 1)
  pure $ row i (M_ mat)


-- | List of columns
columns :: ∀ a. Matrix a -> Array (Vector a)
columns (M_ mat) = do 
  i <- A.range 0 (mat.ncols - 1)
  pure $ column i (M_ mat)


{-- scale :: Double -> M -> M --}
{-- scale s = M.scaleMatrix s --}

{-- diagonal :: [Double] -> M --}
{-- diagonal d = M.diagonalList (length d) 0.0 d --}

{-- chol :: M -> M --}
{-- chol a = M.cholDecomp a --}


{-- -- This is the type of our Inv error representation. --}
{-- data InvError = Err { quality::Double, reason::String } --}

{-- -- We make it an instance of the Error class --}
{-- instance Error InvError where --}
{--   noMsg    = Err 0 "Inversion Error" --}
{--   strMsg s = Err 0 s --}


{-- -- For our monad type constructor, we use Either InvError --}
{-- -- which represents failure using Left InvError or a --}
{-- -- successful result of type a using Right a. --}
{-- type InvMonad = Either InvError --}

{-- invm :: M -> InvMonad M --}
{-- invm m = case invsm m of --}
{--             Right m'  -> return m' --}
{--             Left s    -> throwError (Err 0 ("In Matrix.invm: " ++ s)) -- `debug` "yyyyyyyy" --}

{-- -- inverse of a square matrix, from Data.Matrix with fix --}
{-- --   Uses naive Gaussian elimination formula. --}
{-- invsm ::  M -> Either String M --}
{-- invsm m = rref'd >>= return . M.submatrix 1 n (n + 1) (n * 2) where --}
{--             n = M.nrows m --}
{--             adjoinedWId = m M.<|> M.identity n --}
{--             rref'd = rref adjoinedWId --}

{-- rref :: M -> Either String M --}
{-- rref m = rm where --}
{--     rm = case ref m of --}
{--            Right r -> rrefRefd r --}
{--            Left s -> Left s --}
{--     rrefRefd mtx --}
{--       | M.nrows mtx == 1    = Right mtx --}
{--       | otherwise = --}
{--             let --}
{--                 resolvedRight = foldr (.) id (map resolveRow [1..col-1]) mtx --}
{--                     where --}
{--                     col = M.nrows mtx --}
{--                     resolveRow n = M.combineRows n (-M.getElem n col mtx) col --}
{--                 top = M.submatrix 1 (M.nrows resolvedRight - 1) 1 (M.ncols resolvedRight) resolvedRight --}
{--                 top' = rrefRefd top --}
{--                 bot = M.submatrix (M.nrows resolvedRight) (M.nrows resolvedRight) 1 (M.ncols resolvedRight) resolvedRight --}
{--             in top' >>= return . (M.<-> bot) --}

{-- ref :: M -> Either String M --}
{-- ref mtx --}
{--         | M.nrows mtx == 1 --}
{--             = Right clearedLeft --}
{--         | goodRow == 0 --}
{--             = Left ("In Matrix.ref: Attempt to invert a non-invertible matrix") -- `debug` "xxxxxxxx" --}
{--         | otherwise = --}
{--             let --}
{--                 (tl, tr, bl, br) = M.splitBlocks 1 1 clearedLeft --}
{--                 br' = ref br --}
{--             in case br' of --} 
{--                   Right br'' -> Right ((tl M.<|> tr) M.<-> (bl M.<|> br'')) --}
{--                   Left s -> Left s --}
{--     where --}
{--       goodRow = case listToMaybe (filter (\i -> M.getElem i 1 mtx /= 0) [1..M.nrows mtx]) of -- ERROR in orig: ncols --}
{--                   Nothing   -> 0 --}
{--                   Just x -> x --}
{--       sigAtTop = M.switchRows 1 goodRow mtx --}
{--       normalizedFirstRow = M.scaleRow (1 / M.getElem 1 1 mtx) 1 sigAtTop --}
{--       clearedLeft = foldr (.) id (map combinator [2..M.nrows mtx]) normalizedFirstRow where --}
{--         combinator n = M.combineRows n (-M.getElem n 1 normalizedFirstRow) 1 --}


{-- inv' :: M -> M --}
{-- inv' m = either invErr id (M.inverse m) --}
{--   where invErr s = (M.identity $ M.nrows m) `debug` ("🚩" ++ s) --}

{-- inv''' :: M -> M --}
{-- inv''' m = f e where --}
{--   e = M.inverse m --}
{--   f :: Either String M -> M --}
{--   f (Left s) = (M.identity $ M.nrows m) `debug` ("🚩" ++ s) -- can't invert --}
{--   f (Right m') = fdeb mx where --}
{--     mxx = M.elementwise (/) --}
{--                       (M.elementwise (-) (M.multStd2 m m') (M.identity (M.nrows m))) --}
{--                       m --}
{--     mx = (* 1000.0) . maximum  . M.getMatrixAsVector $ mxx --}
{--     fdeb mx --}
{--       | mx < 1.0 = m' --}
{--       | otherwise = let --}
{--                         sx :: String; sx = printf "%8.3f" (mx :: Double) --}
{--                     in m' `debug` ("^" ++ "inv: max " ++ sx ++ " permille" ) --}

