module Test.Random ( doRandom ) where

import Prelude
{-- import Control.Parallel.Strategies --}
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Random ( )
import Data.Random.Normal
import Statistics.Sample ( mean, variance )

{-- import Math.Probability --}

import Data.Traversable ( mapAccumL )
import Data.Cov ( chol )

import FV.Types ( XMeas (..), VHMeas (..), HMeas (..), Prong (..), MMeas (..)
             , invMass, fromQMeas, fitMomenta)
import FV.Fit ( fit )

{-- import qualified Graphics.Gnuplot.Frame.OptionSet as Opts --}
{-- import Graphics.Histogram --}

doRandom :: forall e. Int -> VHMeas -> Eff (console :: CONSOLE | e) Unit
doRandom cnt vm = do
  log $ "Fit Mass  " <> (show <<< invMass <<< map fromQMeas <<< fitMomenta <<< fit $ vm)

{--   {1-- g <- newStdGen --1} --}
{--   let hf :: Array Number --}
{--       hf = V.fromList --}
{--           {1-- <<< withStrategy (parBuffer 100 rdeepseq) --1} --}
{--           <<< map fitm --}
{--           <<< take cnt <<< gen vm <<< normals $ g -- produce a list of cnt randomized VHMeas --}
{--       (Tuple mean var) = meanVariance hf --}
{--   log $ "Mean Mass " <> show (MMeas mean (sqrt var)) --}
{--   {1-- let hist = histogram binSturges (V.toList hf) --1} --}
{--   {1-- _ <- plot "invMass.png" hist --1} --}
{--   pure unit --}

{-- gen :: VHMeas -> Array Number -> Array VHMeas --}
{-- gen v rs = v' : gen v rs' where --}
{--   (Tuple v' rs') = randVH v rs --}

{-- -- calc fitted invariant mass of VHMeas --}
{-- fitm :: VHMeas -> Double --}
{-- fitm vm = m where --}
{--   (MMeas m _) = invMass <<< map fromQMeas <<< fitMomenta $ fit vm --}

{-- -- randomize the helices in the supplied VHMeas --}
{-- -- and return randomized VHMeas and remaining randoms list --}
{-- randVH :: VHMeas -> Array Number -> (Tuple VHMeas (Array Number)) --}
{-- randVH (VHMeas v hl) rs = (Tuple (VHMeas v hl') rs') where --}
{--   (Tuple rs' hl') = mapAccumL randH rs hl --}

{-- -- randomize a single helix parameters measurement, based on the cov matrix --}
{-- -- return randomized helix and "remaining" random numbers --}
{-- randH :: Array Number -> HMeas -> (Tuple (Array Number) HMeas) --}
{-- randH (r0:r1:r2:r3:r4:rs) (HMeas h hh w0) = (Tuple rs (HMeas h' hh w0)) where --}
{--   h' = v5 $ zipWith (+) (toArray h) (toArray (chol hh ||| fromArray [r0,r1,r2,r3,r4])) --}



