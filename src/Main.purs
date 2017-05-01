module Main where
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)

import Test.Main (main) as T

main :: forall e.  Eff ( console :: CONSOLE
                       --, exception :: EXCEPTION
                       --, fs :: FS
                       | e) Unit
main = do
  T.main

