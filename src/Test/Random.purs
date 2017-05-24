module Test.Random ( testRandom ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console ( CONSOLE, log, logShow )
import Control.Monad.Eff.Random ( random, RANDOM )
import Data.List ( List ( Nil ), (:), range,  concat, take, length )
import Data.Array ( fromFoldable, zipWith )
import Data.Tuple ( Tuple (..) )
import Data.Traversable ( mapAccumL, Accum (..), for )
import Data.Int ( toNumber, ceil )
import Math ( log, sqrt, pi ) as Math
import Statistics.Sample (mean, variance)

import Data.Cov ( chol, fromArray, toArray, (|||) )
import FV.Fit ( fit )
import FV.Types ( XMeas(..), VHMeas(..), HMeas(..), Prong(..), MMeas(..), invMass, fromQMeas, fitMomenta )
import Stuff

{-- import qualified Graphics.Gnuplot.Frame.OptionSet as Opts --}
{-- import Graphics.Histogram --}

-- | generate a list of n normally distributed random values
-- | usinging the Box-Muller method and the random function
normals :: forall e. Int -> Eff (random :: RANDOM | e) (List Number)
normals n = rs where
  ns = range 0 (ceil ((toNumber n)/2.0-1.0))
  rs = do
    lln <- for ns \nn -> do
          u1 <- random
          u2 <- random
          let n1 = Math.sqrt (-2.0 * Math.log u1)
              n2 = 2.0 * Math.pi * u2
          pure $ (n1 : n2 : Nil)
    pure $ concat lln

testRandom :: forall e. Int 
              -> VHMeas 
              -> Eff (console :: CONSOLE, random :: RANDOM | e) Unit
testRandom cnt vm = do
  log $ "Fit Mass  " <> (show <<< invMass <<< map fromQMeas
                        <<< fitMomenta <<< fit $ vm)
  rs <- normals (50)
  logShow $ length rs
  logShow $ rs
  let
      hf :: Array Number
      hf = fromFoldable
          {-- <<< withStrategy (parBuffer 100 rdeepseq) --}
          <<< map fitm
          <<< take cnt <<< gen vm $ rs -- produce a list of cnt randomized VHMeas
      m =  mean hf
      v =  Math.sqrt (variance hf)
  log $ "Mean Mass " <> show (MMeas {m: m, dm: v})
  {-- let hist = histogram binSturges (V.toList hf) --}
  {-- _ <- plot "invMass.png" hist --}
  pure unit

gen :: VHMeas -> List Number -> List VHMeas
gen v rs = v' : gen v rs' where
  (Tuple v' rs') = randVH v rs

-- calc fitted invariant mass of VHMeas
fitm :: VHMeas -> Number
fitm vm = m where
  MMeas {m: m} = invMass <<< map fromQMeas <<< fitMomenta $ fit vm

-- randomize the helices in the supplied VHMeas
-- and return randomized VHMeas and remaining randoms list
randVH :: VHMeas -> List Number -> (Tuple VHMeas (List Number))
randVH (VHMeas {vertex: v, helices: hl}) rs = (Tuple (VHMeas {vertex: v, helices: hl'}) rs') where
  {accum: rs', value: hl'} = mapAccumL randH rs hl

-- randomize a single helix parameters measurement, based on the cov matrix
-- return randomized helix and "remaining" random numbers
randH :: List Number -> HMeas -> Accum (List Number) HMeas
randH (r0:r1:r2:r3:r4:rs) (HMeas h hh w0) = {accum: rs, value: (HMeas h' hh w0)} where
  h' = fromArray $ zipWith (+) (toArray h) (toArray (chol hh ||| fromArray [r0,r1,r2,r3,r4]))
randH _ _ = undefined
