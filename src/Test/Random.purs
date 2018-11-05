module Test.Random ( testRandom ) where

import Prelude.Extended ( normals, stats, show , bind, discard, pure, map, Unit, unit
  , (<<<) , ($), (+), (<>), (<$>)
  )
import Effect ( Effect )
import Effect.Console ( log )
import Data.List.Lazy ( replicateM )
import Data.Array ( fromFoldable, zipWith ) as A
import Data.Traversable ( for )
import Data.Tuple ( Tuple (..) )

import Data.Cov ( chol, fromArray, toArray, (*.), Vec5 )
import FV.Fit ( fit )
import FV.Types ( VHMeas(..), HMeas(..), MMeas(..)
  , invMass, fromQMeas, fitMomenta )

{-- import qualified Graphics.Gnuplot.Frame.OptionSet as Opts --}
{-- import Graphics.Histogram --}

--| Randomizable TypeClass to provide randomize method
--| for MC smearing of a measurement
class Randomizable a where
  randomize :: a -> Effect a
--| randomize a single helix parameters measurement, based on the cov matrix
--| return randomized helix
instance randomizableHMeas :: Randomizable HMeas where
  randomize (HMeas h hh w0) = do
    rs <- normals 5
    let h' = fromArray $ A.zipWith (+) (toArray h)
                                       (toArray (chol hh *. (fromArray rs)::Vec5))
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

testRandom :: Int -> VHMeas -> Effect Unit
testRandom cnt vm = do
  log $ "Fit Mass  " <> (show <<< invMass <<< map fromQMeas
                        <<< fitMomenta <<< fit $ vm)
  ms <- replicateM cnt $ fitm <$> (randomize vm)
  let (Tuple m dm) = stats $ A.fromFoldable ms
  log $ "Mean Mass " <> show (MMeas {m, dm})
  {-- let hist = histogram binSturges (V.toList hf) --}
  {-- _ <- plot "invMass.png" hist --}
  pure unit

