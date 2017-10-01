module Main where
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console ( CONSOLE )
import Control.Monad.Eff.Random ( RANDOM )
import Test.Assert (ASSERT)

import Test.Main (main) as T

main :: forall e.  Eff ( console :: CONSOLE
                       , assert :: ASSERT
                       , random :: RANDOM
                       --, exception :: EXCEPTION
                       --, fs :: FS
                       | e) Unit
main = do
  T.main
