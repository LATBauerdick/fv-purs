module Test.Random ( testRandom ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console ( CONSOLE, log )
import Control.Monad.Eff.Random ( RANDOM )
import Data.List.Lazy ( replicateM )
import Data.Array ( fromFoldable, zipWith ) as A
import Data.Traversable ( for )
import Statistics.Sample (mean, stddev)

import Data.Cov ( chol, fromArray, toArray, (|||) )
import FV.Fit ( fit )
import FV.Types ( VHMeas(..), HMeas(..), MMeas(..)
  , invMass, fromQMeas, fitMomenta )
import Stuff ( normals )

{-- import qualified Graphics.Gnuplot.Frame.OptionSet as Opts --}
{-- import Graphics.Histogram --}

--| Randomizable TypeClass to provide randomize method
--| for MC smearing of a measurement
class Randomizable a where
  randomize :: forall e. a -> Eff (random :: RANDOM | e) a
--| randomize a single helix parameters measurement, based on the cov matrix
--| return randomized helix
instance randomizableHMeas :: Randomizable HMeas where
  randomize (HMeas h hh w0) = do
    rs <- normals 5
    let h' = fromArray $ A.zipWith (+) (toArray h)
                                       (toArray (chol hh ||| fromArray rs))
    pure $ HMeas h' hh w0

--| randomize a vertex measurement by randomizing each helix parameter measurement
--| leaving the initial vertex untouched
instance randomizableVHMeas :: Randomizable VHMeas where
  randomize (VHMeas { vertex: v, helices: hl}) = do
        hl' <- for hl \h -> randomize h
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

