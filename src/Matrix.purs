module Matrix (
    Matrix (..)
  , M (..), M5 (..), V5 (..)
  , sw, fromList, fromList2
  , zero, identity
  ) where

import Prelude
import Data.Array (
                    range, cons, index, unsafeIndex
                  , mapMaybe, replicate, slice, length , all
                  ) as A
import Data.Tuple (Tuple (..))
import Data.Maybe ( Maybe(..), fromJust, fromMaybe )
import Data.Ord (signum)
import Partial.Unsafe (unsafePartial)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Monoid ( class Monoid, mempty )

newtype M0 = M0 (Matrix Number)
instance showM0 :: Show M0 where
  show (M0 m) = prettyMatrix m

type M = Matrix Number
newtype V5 = V5 M
newtype M5 = M5 M

instance showV5 :: Show V5 where
  show _ = "show V5: T.B.I."

-- | sandwich a matrix: a^T * b * a
sw :: M -> M -> M
sw a b = a
{-- sw a b = (tr a) * b * a --}
{-- tr :: M -> M --}
{-- tr = transpose --}
{-- fromList :: Int -> Array Number -> M0 --}
{-- fromList r ds = M0 (unsafePartial $ fromJust $ fromArray r 1 ds) --}
{-- fromList2 :: Int -> Int -> Array Number -> M0 --}
{-- fromList2 r c ds = M0 (unsafePartial $ fromJust $ fromArray r c ds) --}

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
values (M_ {values: v}) = v

encode :: Int -> Int -> Int -> Int
encode m i j = (i-1)*m + j - 1

decode :: Int -> Int -> (Tuple Int Int)
decode m k = (Tuple (q+1) (r+1))
  where (Tuple q r) = quotRem k m

quotRem :: Int -> Int -> (Tuple Int Int)
quotRem n d = if signum r == - signum d
                 then (Tuple (q+1) (r-d))
                 else qr
  where qr@(Tuple q r) = divMod n d

divMod :: Int -> Int -> (Tuple Int Int)
divMod n d = (Tuple (n `div` d) (n `mod` d))


instance eqMatrix :: Eq a => Eq (Matrix a) where
  eq m1 m2 =
    let (M_ {nrows: r1, ncols: c1}) = m1
        (M_ {nrows: r2, ncols: c2}) = m2
        et = do
          i <- A.range 1 r1
          j <- A.range 1 c1
          pure $ getElem i j m1 == getElem i j m2
    in (r1 == r2) && (c1 == c2) && (A.all id et)

-- | Just a cool way to output the size of a matrix.
sizeStr :: Int -> Int -> String
sizeStr n m = show n <> "x" <> show m

-- | Display a matrix as a 'String' using the 'Show' instance of its elements.
prettyMatrix :: forall a. Show a => Matrix a -> String
prettyMatrix (M_ m) = show m.values
--prettyMatrix m@(M_ {values: v} = unlines
--  [ "( " <> unwords (fmap (\j -> fill mx $ show $ m ! (i,j)) [1..ncols m]) <> " )" | i <- [1..nrows m] ]
--  where
--    mx = V.maximum $ fmap (length . show) v
--    fill k str = replicate (k - length str) ' ' ++ str

instance showMatrix :: Show a => Show (Matrix a) where
  show = prettyMatrix


instance functorMatrix :: Functor Matrix where
  map f (M_ {nrows: r, ncols: c, values: v}) = M_ {nrows: r, ncols: c, values: map f v}

{-- instance monoidMatrix :: Monoid a => Monoid (Matrix a) where --}
{--   mempty = fromList 1 1 [mempty] --}
{--   append m m' = matrix (max (nrows m) (nrows m')) (max (ncols m) (ncols m')) $ uncurry zipTogether --}
{--     where zipTogether row column = fromMaybe mempty $ safeGet row column m <> safeGet row column m' --}

-- | /O(rows*cols)/. The transpose of a matrix.
--   Example:
--
-- >           ( 1 2 3 )   ( 1 4 7 )
-- >           ( 4 5 6 )   ( 2 5 8 )
-- > transpose ( 7 8 9 ) = ( 3 6 9 )
{-- transpose :: Matrix a -> Matrix a --}
{-- transpose m = matrix (ncols m) (nrows m) $ \(i,j) -> m ! (j,i) --}

-- | Create column vector from array
fromList :: forall a. Int -> Array a -> Matrix a
fromList r vs = M_ {nrows: r, ncols: r, values: vs}
-- | Create matrix from array
fromList2 :: forall a. Int -> Int -> Array a -> Matrix a
fromList2 r c vs = M_ {nrows: r, ncols: c, values: vs}
getElem :: ∀ a.
           Int      -- ^ Row
        -> Int      -- ^ Column
        -> Matrix a -- ^ Matrix
        -> a
getElem i j m =
  case safeGet i j m of
    Just x -> x
    Nothing -> unsafeThrow
      $ "getElem: Trying to get the "
      <> show ( Tuple i j )
      <> " element from a "
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

unsafeGet i j (M_ {nrows: r, values: v}) = unsafePartial $ A.unsafeIndex v $ encode w (i+ro) (j+co)
  where w = r
        ro = 0
        co = 0


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
zero :: 
     Int -- ^ Rows
  -> Int -- ^ Columns
  -> Matrix Number
zero n m = M_ { nrows: n, ncols: m, values: (A.replicate (n*m) 0.0)}

-- | /O(rows*cols)/. Generate a matrix from a generator function.
--   Example of usage:
--
-- >                                  (  1  0 -1 -2 )
-- >                                  (  3  2  1  0 )
-- >                                  (  5  4  3  2 )
-- > matrix 4 4 $ \(i,j) -> 2*i - j = (  7  6  5  4 )
{-- matrix :: Int -- ^ Rows --}
{--        -> Int -- ^ Columns --}
{--        -> ((Int,Int) -> a) -- ^ Generator function --}
{--        -> Matrix a --}
{-- matrix n m f = M_ {nrows: n, ncols: m, values: val} where --}
{--   val = A.create $ do --}
{--     v <- MV.new $ n * m --}
{--     let en = encode m --}
{--     numLoop 1 n $ --}
{--       \i -> numLoop 1 m $ --}
{--       \j -> MV.unsafeWrite v (en (i,j)) (f (i,j)) --}
{--     return v --}

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
    pure $ if i==j then 1.0 else 0.0

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


-- vectors are column-wise, represented as matrix of dimension nx1
{-- sub :: Int -> M -> M --}
{-- sub n v = M.submatrix 1 n 1 1 v --}
{-- sub2 :: Int -> M -> M --}
{-- sub2 n m = M.submatrix 1 n 1 n m --}
{-- scalar :: M -> Double --}
{-- scalar m = m M.! (1,1) --}

{-- toList :: Int -> M -> [Double] --}
{-- toList n m = take n $ M.toList m --}


{-- scale :: Double -> M -> M --}
{-- scale s = M.scaleMatrix s --}

{-- diagonal :: [Double] -> M --}
{-- diagonal d = M.diagonalList (length d) 0.0 d --}

{-- scaleDiag :: Double -> M -> M --}
{-- scaleDiag s = (M.diagonal 0.0 . M.getDiag . M.scaleMatrix  s) --}

{-- (^+) :: M -> M --}
{-- (^+) = M.transpose --}


{-- chol :: M -> M --}
{-- chol a = M.cholDecomp a --}

{-- det :: M -> Double --}
{-- det m = M.detLU m --}

{-- -- This is the type of our Inv error representation. --}
{-- data InvError = Err { quality::Double, reason::String } --}

{-- -- We make it an instance of the Error class --}
{-- instance Error InvError where --}
{--   noMsg    = Err 0 "Inversion Error" --}
{--   strMsg s = Err 0 s --}

{-- invMaybe :: M -> Maybe M --}
{-- invMaybe m = case invsm m of --}
{--                Right im -> Just im --}
{--                Left s -> Nothing `debug` ("Error in Matrix.invsm: " ++ s) --}

{-- inv :: M -> M --}
{-- inv m =  let (Right m') = do { invm m } `catchError` printError --}
{--           in m' --}
{--          where --}
{--            one = (M.identity $ M.nrows m) --}
{--            printError :: InvError -> InvMonad M --}
{--            printError e = return one `debug` ("Error in Matrix.inv: " ++ (show (quality e)) ++ ": " ++ (reason e)) --}

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

