module Stuff where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Math ( sqrt )
import Data.Ord (signum)
import Data.String ( takeWhile, dropWhile, toCharArray, fromCharArray, split, Pattern (..) )
import Data.Char (toCharCode)
import Data.List ( List(..), (:))
import Data.Array ( mapMaybe, unsafeIndex, range, length )
import Data.Tuple ( Tuple(..), fst, snd )
import Data.Maybe ( Maybe(..), fromMaybe', fromJust )
import Data.Foldable ( class Foldable, foldr )
import Partial.Unsafe (unsafePartial, unsafePartialBecause, unsafeCrashWith)
import Unsafe.Coerce ( unsafeCoerce ) as Unsafe.Coerce
import Data.List ( fromFoldable )
import Data.Int ( round, toNumber, floor )
import Text.Format ( format, precision, width )
import Data.Enum ( class Enum )
import Control.MonadZero ( guard )
import Control.Monad.Eff.Unsafe (unsafePerformEff)

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
trace s a = const a (unsafePerformEff (log s))
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
uidx = unsafePartial unsafeIndex

uJust :: forall a. Maybe a -> a
uJust = unsafePartial $ fromJust

-- | filter list of objects given list of indices in [a]
-- | return list with only those b that have  indices that  are in rng [a]
iflt :: forall a. Array Int -> Array a  -> Array a
iflt rng hl = do
  i <- rng
  pure $ uidx hl i

-- | remove element at index
irem :: forall a. Int -> Array a -> Array a
irem indx hl = do
  i <- range 0 ((length hl)-1)
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
words s = case dropWhile isSpace s of
                                "" -> Nil
                                str' -> let s0 = takeWhile (not isSpace) str'
                                            s1 = dropWhile (not isSpace) str'
                                        in s0 : words s1

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
break _ Nil             =  (Tuple Nil Nil)
break p xs@(x:xs')
           | p x        =  (Tuple Nil xs)
           | otherwise  =  let yszs = break p xs' 
                               ys = fst yszs
                               zs = snd yszs
                           in (Tuple (x:ys) zs)

-- | 'unwords' is an inverse operation to 'words'.
-- It joins words with separating spaces.
unwords                 :: List String -> String
unwords Nil             =  ""
unwords ws              =  foldr1 (\w s -> w <> " " <> s) ws

-- | 'unlines' is an inverse operation to 'lines'.
-- It joins lines, after appending a terminating newline to each.
unlines                 :: List String -> String
unlines Nil = ""
unlines (l:ls) = l <> "\n" <> unlines ls

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

