-- | Functions for working with PureScripts builtin `Number` type.
module Data.Number
  ( fromString
  {-- , nan --}
  {-- , isNaN --}
  {-- , infinity --}
  , isFinite
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Global ( isFinite ) as G

fromString :: String -> Maybe Number
fromString = fromStringImpl Just Nothing >>> check
  where
    check num = case num of
                     Just n -> if (isFinite n) then Just n else Nothing
                     Nothing -> Nothing

foreign import fromStringImpl
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> String
  -> Maybe Number

{-- -- | Not a number (NaN). --}
{-- nan ∷ Number --}
{-- nan = G.nan --}

{-- -- | Test whether a `Number` is NaN. --}
{-- isNaN ∷ Number → Boolean --}
{-- isNaN = G.isNaN --}

{-- -- | Positive infinity. --}
{-- infinity ∷ Number --}
{-- infinity = G.infinity --}

-- | Test whether a number is finite.
isFinite ∷ Number → Boolean
isFinite = G.isFinite

