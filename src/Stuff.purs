module Stuff where

import Prelude
import Math ( sqrt )
import Data.Ord (signum)
import Data.List ( List(..), (:), dropWhile )
import Data.Tuple ( Tuple(..), fst, snd )
import Data.Maybe ( Maybe(..), fromMaybe' )
import Data.Foldable ( class Foldable, foldr )
import Partial.Unsafe (unsafePartial, unsafePartialBecause, unsafeCrashWith)
import Unsafe.Coerce ( unsafeCoerce ) as Unsafe.Coerce
import Data.Char.Unicode ( isSpace )

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

{-- -- | 'words' breaks a string up into a list of words, which were delimited --}
{-- -- | by white space. --}
{-- words                   :: String -> List String --}
{-- words s                 =  case dropWhile isSpace s of --}
{--                                 "" -> Nil --}
{--                                 ss -> let --}
{--                                           ws = break isSpace ss --}
{--                                           w = fst ws --}
{--                                           s'' = snd ws --}
{--                                       in w : words s'' --}

{-- -- | 'break', applied to a predicate @p@ and a list @xs@, returns a tuple where --}
{-- -- | first element is longest prefix (possibly empty) of @xs@ of elements that --}
{-- -- | /do not satisfy/ @p@ and second element is the remainder of the list: --}
{-- -- --}
{-- -- > break (> 3) [1,2,3,4,1,2,3,4] == ([1,2,3],[4,1,2,3,4]) --}
{-- -- > break (< 9) [1,2,3] == ([],[1,2,3]) --}
{-- -- > break (> 9) [1,2,3] == ([1,2,3],[]) --}
{-- -- --}
{-- -- 'break' @p@ is equivalent to @'span' ('not' . p)@. --}

{-- break                   :: forall a. (a -> Boolean) -> List a -> (Tuple (List a) (List a)) --}
{-- -- HBC version (stolen) --}
{-- break _ Nil           =  (Tuple Nil Nil) --}
{-- break p xs@(x:xs') --}
{--            | p x        =  (Tuple Nil xs) --}
{--            | otherwise  =  let yszs = break p xs' --} 
{--                                ys = fst yszs --}
{--                                zs = snd yszs --}
{--                            in (Tuple (x:ys) zs) --}

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
