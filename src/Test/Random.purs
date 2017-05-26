module Test.Random ( testRandom ) where

import Prelude
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Console ( CONSOLE, log, logShow )
import Control.Monad.Eff.Random ( random, RANDOM )
import Data.List ( List ( Nil ), (:), range,  concat, take, length )
import Data.List.Lazy ( replicateM )
import Data.Array ( fromFoldable, zipWith, range ) as A
import Data.Tuple ( Tuple (..) )
import Data.Traversable ( mapAccumL, Accum (..), for )
import Data.Int ( toNumber, ceil )
import Math ( log, sqrt, pi, sin, cos ) as Math
import Statistics.Sample (mean, stddev)

import Data.Cov ( chol, fromArray, toArray, (|||) )
import FV.Fit ( fit )
import FV.Types ( XMeas(..), VHMeas(..), HMeas(..), Prong(..), MMeas(..)
  , invMass, fromQMeas, fitMomenta )
import Stuff

{-- import qualified Graphics.Gnuplot.Frame.OptionSet as Opts --}
{-- import Graphics.Histogram --}

-- | generate a list of n normally distributed random values
-- | usinging the Box-Muller method and the random function
normals :: forall e. Int -> Eff (random :: RANDOM | e) (List Number)
normals n = rs where
  mx = ceil ((toNumber n)/2.0-1.0)
  ns = range 0 mx
  rs = do
    lln <- for ns \nn -> do
          u1 <- random
          u2 <- random
          let r = Math.sqrt (-2.0 * Math.log u1)
              t = 2.0 * Math.pi * u2
          pure $ (r * Math.cos t : r * Math.sin t : Nil)
    pure $ concat lln


-- randomize a single helix parameters measurement, based on the cov matrix
-- return randomized helix and "remaining" random numbers
rrandH :: List Number -> HMeas -> HMeas
rrandH rs (HMeas h hh w0) = HMeas h' hh w0 where
  h' = fromArray $ A.zipWith (+) (toArray h)
                                 (toArray (chol hh ||| fromArray (A.fromFoldable $ take 5 rs)))

randomize :: forall e. VHMeas -> Eff (random :: RANDOM | e) VHMeas
randomize (VHMeas { vertex: v, helices: hl}) = do
        hl' <- for hl \h -> do
                              rs <- normals (5)
                              pure $ rrandH rs h
        pure $ VHMeas { vertex: v, helices: hl' }

-- calc fitted invariant mass of VHMeas
fitm :: VHMeas -> Number
fitm vm = m where
  MMeas {m: m} = invMass <<< map fromQMeas <<< fitMomenta $ fit vm

testRandom :: forall e. Int
              -> VHMeas
              -> Eff (console :: CONSOLE, random :: RANDOM | e) Unit
testRandom cnt vm = do
  log $ "Fit Mass  " <> (show <<< invMass <<< map fromQMeas
                        <<< fitMomenta <<< fit $ vm)
  ms <- replicateM cnt $ fitm <$> (randomize vm)
  let m  = mean $ A.fromFoldable ms
      dm = stddev $ A.fromFoldable ms
  log $ "Mean Mass " <> show (MMeas {m, dm})
  {-- let hist = histogram binSturges (V.toList hf) --}
  {-- _ <- plot "invMass.png" hist --}
  pure unit

