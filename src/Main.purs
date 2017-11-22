module Main where
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console ( CONSOLE, log, logShow )
import Control.Monad.Eff.Random ( RANDOM )
import Control.Monad.Eff.Exception ( EXCEPTION )
import Test.Assert (ASSERT)

import Node.FS (FS)
import Data.List ( mapMaybe )
import Data.Number ( fromString )
import Stuff ( iflt, to1fix, words, uJust )

import Test.Main (main) as T

main :: forall e.  Eff ( console :: CONSOLE
                       {-- , assert :: ASSERT --}
                       , random :: RANDOM
                       , exception :: EXCEPTION
                       , fs :: FS
                       | e) Unit
main = do
  T.main
