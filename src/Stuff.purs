module Stuff where

import Prelude
import Math ( sqrt )
import Data.Ord (signum)
import Data.List ( List(..), (:) )
import Data.Tuple ( Tuple(..) )
import Data.Maybe ( Maybe(..), fromMaybe' )
import Data.Foldable ( class Foldable, foldr )
import Partial.Unsafe (unsafePartial, unsafePartialBecause, unsafeCrashWith)
import Unsafe.Coerce ( unsafeCoerce ) as Unsafe.Coerce

-- | simultaneous 'quot' and 'rem'
quotRem :: Int -> Int -> (Tuple Int Int)
--quotRem             :: a -> a -> (a,a)
quotRem n d = if signum r == - signum d
                 then (Tuple (q+1) (r-d))
                 else qr
  where qr@(Tuple q r) = divMod n d

-- | simultaneous 'div' and 'mod'
divMod :: Int -> Int -> (Tuple Int Int)
--divMod              :: a -> a -> (a,a)
divMod n d = (Tuple (n `div` d) (n `mod` d))

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

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w*w + h*h)

gcd' :: Int -> Int -> Int
gcd' n 0 = n
gcd' 0 m = m
gcd' n m | n > m = gcd' (n - m) m
         | otherwise = gcd' n (m - n)
