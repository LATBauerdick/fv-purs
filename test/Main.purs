module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Data.Tuple (fst, snd)

import Stuff (gcd', diagonal)
import Test.Input (hSlurp, hSlurp')


main :: forall e.  Eff ( exception :: EXCEPTION 
                       , console :: CONSOLE 
                       , fs :: FS | e
                       ) Unit
main = void $ launchAff do
  log "FVT Test Suite"
  logShow (diagonal 3.0 4.0)
  logShow $ gcd' 121 22

  log "Does hSlurp work??"
  nums <- hSlurp "dat/tr05129e001412.dat"
  logShow nums
  nums <- hSlurp "dat/tav-4.dat"
  logShow nums
  {-- (VHMeas _ hl, _) <- hSlurp "dat/tav-0.dat" --}
  {-- let HMeas _ _ w = head hl --}
  {-- w `shouldBe` 0.0114 --}

