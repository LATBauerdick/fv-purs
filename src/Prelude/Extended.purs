module Prelude.Extended
  ( module Prelude
  , List (..), range, fromList
  , fromIntegral
  , words, unwords, unlines
  , sqr, mod'
  , irem, iflt
  , prettyMatrix
  , normals, stats
  , debug, trace
  , uidx, uJust
  , to1fix, to2fix, to3fix, to5fix
  , error
  , undefined
  ) where

import Prelude
import Effect ( Effect )
import Effect.Console (log)
import Effect.Random ( random )
import Math ( log, sqrt, pi, sin, cos ) as Math
import Data.Ord (signum)
{-- import Data.String ( takeWhile, dropWhile, split, Pattern (..) ) --}
import Data.String.CodeUnits ( fromCharArray )
import Data.String ( length ) as S
import Data.String.Extra ( words ) as Data.String.Extra
import Data.Char (toCharCode)
import Data.List ( List(..), (:), range, fromFoldable ) as L
import Data.Array ( unsafeIndex, range, length, take, concat
  , fromFoldable, replicate
  ) as A
import Data.Unfoldable ( replicateA )
import Data.Tuple ( Tuple(..), fst, snd )
import Data.Maybe ( Maybe(..), fromMaybe', fromMaybe, fromJust )
import Data.Foldable ( class Foldable, foldr, sum, maximum )
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Unsafe.Coerce ( unsafeCoerce ) as Unsafe.Coerce
import Data.Int ( round, toNumber, floor )
import Text.Format ( format, precision, width )
import Control.MonadZero ( guard )
import Effect.Unsafe (unsafePerformEffect)

-- List, PureScript does not provide sugar
type List a = L.List a
range :: Int -> Int -> List Int
range f t = L.range f t
fromList :: forall f. Foldable f => f ~> Array
fromList = A.fromFoldable

fromIntegral :: Int -> Number
fromIntegral = toNumber

{-- -- | Basic numeric class. --}
{-- class  Num a  where --}
{--     {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-} --}

{--     (+), (-), (*)       :: a -> a -> a --}
{--     -- | Unary negation. --}
{--     negate              :: a -> a --}
{--     -- | Absolute value. --}
{--     abs                 :: a -> a --}
{--     -- | Sign of a number. --}
{--     -- The functions 'abs' and 'signum' should satisfy the law: --}
{--     -- --}
{--     -- > abs x * signum x == x --}
{--     -- --}
{--     -- For real numbers, the 'signum' is either @-1@ (negative), @0@ (zero) --}
{--     -- or @1@ (positive). --}
{--     signum              :: a -> a --}
{--     -- | Conversion from an 'Integer'. --}
{--     -- An integer literal represents the application of the function --}
{--     -- 'fromInteger' to the appropriate value of type 'Integer', --}
{--     -- so such literals have type @('Num' a) => a@. --}
{--     fromInteger         :: Integer -> a --}

{--     {-# INLINE (-) #-} --}
{--     {-# INLINE negate #-} --}
{--     x - y               = x + negate y --}
{--     negate x            = 0 - x --}


-- | Returns `True` for any Unicode space character, and the control
-- | characters `\t`, `\n`, `\r`, `\f`, `\v`.
-- |
-- | `isSpace` includes non-breaking space.
-- avoiding to include unicode which increases make time
isSpace :: Char -> Boolean
-- The magic 0x377 used in the code below isn't really that magical. As of
-- 2014, all the codepoints at or below 0x377 have been assigned, so we
-- shouldn't have to worry about any new spaces appearing below there.
isSpace c = if uc <= 0x337
               then uc == 32 || (uc >= 9 && uc <= 13) || uc == 0xa0
               else false
  where
    uc :: Int
    uc = toCharCode c

trace :: forall a. String -> a -> a
trace s a = const a (unsafePerformEffect (log s))

debug :: forall a. a -> String -> a
debug = flip trace
{-- unsafePerformEff :: forall eff a. Eff eff a -> a --}
{-- log :: forall eff. String -> Eff (console :: CONSOLE | eff) Unit --}

-- | square of a number
sqr :: Number -> Number
sqr a = a*a

-- | generalisation of 'div' to any instance of Real
div' :: Number -> Number -> Int
div' n d = floor ( n /  d)

-- | generalisation of 'divMod' to any instance of Real
divMod' :: Number -> Number -> (Tuple Int Number)
divMod' n d = (Tuple f (n - (toNumber f) * d)) where
    f = div' n d

-- | generalisation of 'mod' to any instance of Real
mod' :: Number -> Number -> Number
mod' n d = n - (toNumber f) * d where
    f = div' n d

-- | unsafe index to Array
uidx :: forall a. Array a -> Int -> a
uidx = unsafePartial A.unsafeIndex

uJust :: forall a. Maybe a -> a
uJust = unsafePartial $ fromJust

prettyMatrix :: Int -> Int -> Array Number -> String
prettyMatrix r c v = unlines ls where
  -- | /O(1)/. Unsafe variant of 'getElem', without bounds checking.
  unsafeGet :: Int          -- ^ Row
            -> Int          -- ^ Column
            -> Array Number -- ^ Matrix
            -> Number
  unsafeGet i j vv = unsafePartial $ A.unsafeIndex vv $ encode c i j
  encode :: Int -> Int -> Int -> Int
  encode m i j = (i-1)*m + j - 1
  ls = do
    i <- range 1 r
    let ws :: List String
        ws = map (\j -> fillBlanks mx (to3fix $ unsafeGet i j v)) (range 1 c)
    pure $ "( " <> unwords ws <> " )"
  mx = fromMaybe 0 (maximum $ map (S.length <<< to3fix) v)
  fillBlanks k str =
    (fromCharArray $ A.replicate (k - S.length str) ' ') <> str

-- | filter list of objects given list of indices in [a]
-- | return list with only those b that have  indices that  are in rng [a]
iflt :: forall a. Array Int -> Array a  -> Array a
iflt rng hl = do
  i <- rng
  pure $ uidx hl i

-- | remove element at index
irem :: forall a. Int -> Array a -> Array a
irem indx hl = do
  i <- A.range 0 ((A.length hl)-1)
  guard $ i /= indx
  pure $ uidx hl i


-- | round to 3 decimal
roundDec :: Number -> Number
roundDec x = (toNumber (round ( 1000.0 * x )))/1000.0

to0fix :: Number -> String
to0fix = format (width 4 <> precision 0)
to1fix :: Number -> String
to1fix = format (width 6 <> precision 1)
to2fix :: Number -> String
to2fix = format (width 7 <> precision 2)
to3fix :: Number -> String
to3fix = format (width 8 <> precision 3)
to5fix :: Number -> String
to5fix = format (width 10 <> precision 5)

-- | simultaneous 'quot' and 'rem'
quotRem :: Int -> Int -> (Tuple Int Int)
--quotRem             :: a -> a -> (a,a)
quotRem n d = if signum r == - signum d
                 then (Tuple (q+1) (r-d))
                 else qr
  where qr = divMod n d
        q = fst qr
        r = snd qr

-- | simultaneous 'div' and 'mod'
divMod :: Int -> Int -> (Tuple Int Int)
--divMod              :: a -> a -> (a,a)
divMod n d = (Tuple (n `div` d) (n `mod` d))

-- | 'words' breaks a string up into a list of words, which were delimited
-- | by white space.
words :: String -> List String
words = L.fromFoldable <<< Data.String.Extra.words
{-- words s = case dropWhile isSpace s of --}
{--                                 "" -> L.Nil --}
{--                                 str' -> let s0 = takeWhile (not isSpace) str' --}
{--                                             s1 = dropWhile (not isSpace) str' --}
{--                                         in s0 L.: words s1 --}

-- | 'break', applied to a predicate @p@ and a list @xs@, returns a tuple where
-- | first element is longest prefix (possibly empty) of @xs@ of elements that
-- | /do not satisfy/ @p@ and second element is the remainder of the list:
--
-- > break (> 3) [1,2,3,4,1,2,3,4] == ([1,2,3],[4,1,2,3,4])
-- > break (< 9) [1,2,3] == ([],[1,2,3])
-- > break (> 9) [1,2,3] == ([1,2,3],[])
--
-- 'break' @p@ is equivalent to @'span' ('not' . p)@.

break :: forall a. (a -> Boolean) -> List a -> (Tuple (List a) (List a))
-- HBC version (stolen)
break _ L.Nil             =  (Tuple L.Nil L.Nil)
break p xs@(x L.: xs')
           | p x        =  (Tuple L.Nil xs)
           | otherwise  =  let yszs = break p xs' 
                               ys = fst yszs
                               zs = snd yszs
                           in (Tuple (x L.: ys) zs)

-- | 'unwords' is an inverse operation to 'words'.
-- It joins words with separating spaces.
unwords                 :: List String -> String
unwords L.Nil             =  ""
unwords ws              =  foldr1 (\w s -> w <> " " <> s) ws

-- | 'unlines' is an inverse operation to 'lines'.
-- It joins lines, after appending a terminating newline to each.
unlines                 :: List String -> String
unlines L.Nil = ""
unlines (l L.: ls) = l <> "\n" <> unlines ls

-- | A variant of 'foldr' that has no base case,
-- and thus may only be applied to non-empty structures.
--
-- @'foldr1' f = 'List.foldr1' f . 'toList'@
foldr1 :: forall a t. Foldable t => Show a => (a -> a -> a) -> t a -> a
foldr1 f xs = fromMaybe' ( \_ -> error $ "foldr1: empty structure" <> show xx) xx
                         where
  xx = (foldr mf Nothing xs)
  mf :: a -> Maybe a -> Maybe a
  mf acc m = Just (case m of
                         Nothing -> acc
                         Just y  -> f acc y)

undefined :: forall a. a
undefined = Unsafe.Coerce.unsafeCoerce unit

error :: forall a. String -> a
error = unsafeCrashWith

-- | generate a list of n normally distributed random values
-- | usinging the Box-Muller method and the random function
boxMuller :: Effect (Array Number)
boxMuller = do
              u1 <- random
              u2 <- random
              let r = Math.sqrt (-2.0 * Math.log u1)
                  t = 2.0 * Math.pi * u2
                  b1 = r * Math.cos t
                  b2 = r * Math.sin t
              pure $ [ b1, b2 ]
normals :: Int -> Effect (Array Number)
normals n = do
  ls <- replicateA ((n+1)/2) $ boxMuller
  pure $ A.take n $ A.concat ls

-- | Calculate mean and standard deviation
stats :: Array Number -> Tuple Number Number
stats xs = Tuple mean stddev where
  n = toNumber (A.length xs)
  mean = sum xs / n
  stddev = Math.sqrt $ sum (map (\v -> sqr (v-mean)) xs) / n
