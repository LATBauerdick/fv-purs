module Main where
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Test.Assert (ASSERT, assert)

import Data.Number (fromString)
import Global (readFloat, readInt, isFinite, infinity, nan, isNaN)
import Test.Main (main) as T

main :: forall e.  Eff ( console :: CONSOLE
                       , assert :: ASSERT
                       --, exception :: EXCEPTION
                       --, fs :: FS
                       | e) Unit
main = do
  T.main
  logShow $ fromString "12.34"
  logShow $ fromString "1e4"
  logShow $ fromString "1.34e-5"
  logShow $ fromString "-1.34e"
  logShow $ fromString "bad"
  let num = 12345.6789

  log "nan /= nan"
  assert $ nan /= nan

  log "not (isNaN 6.0)"
  assert $ not (isNaN 6.0)

  log "isNaN nan"
  assert $ isNaN nan

  log "infinity > 0.0"
  assert $ infinity > 0.0

  log "-infinity < 0.0"
  assert $ -infinity < 0.0

  log "not (isFinite infinity)"
  assert $ not (isFinite infinity)

  log "isFinite 0.0"
  assert $ isFinite 0.0

  log "readInt 16 \"0xFF\" == 255.0"
  assert $ readInt 16 "0xFF" == 255.0

  log "readInt 10 \"3.5\" == 3.0"
--  assert $ readInt 10 "3.5" == 3.0

  log "readFloat \"3.5\" == 3.5"
  assert $ readFloat "3.5" == 3.5
