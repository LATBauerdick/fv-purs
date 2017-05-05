
module Test.Input ( hSlurp, hSlurpMCtruth ) where

import Prelude
import Control.MonadZero (guard)
import Data.Number ( fromString )
import Data.Int (fromString, round) as Data.Int
import Data.Array ( take, drop, (!!), fromFoldable, mapMaybe, range, slice, head )
import Data.Tuple (Tuple (..), fst)
import Data.Maybe ( Maybe (Nothing, Just) )
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Plus (empty)

import Math ( sin, cos )

-- import Unsafe.Coerce (unsafeCoerce)
import Partial.Unsafe (unsafePartial)

import Data.Matrix (sw, fromList, fromList2)
import FV.Types ( V5 (..), M5 (..), MCtruth (..), VHMeas (..), XMeas (..), HMeas (..) )
import Stuff ( words )


type FilePath = String -- from Node.Path

{-- listToArray :: forall a. List a -> Array a --}
{-- listToArray = ?whatGoesHere --}

-- slurp in a String of measurements of PU z-positions into MCtruth
hSlurpMCtruth :: String -> Maybe MCtruth
hSlurpMCtruth ds = mc where
  ws = words ds
  npu :: Maybe Int
  npu = do
              key <- head ws
              guard $ key == "PU_zpositions:"
              snpu <- ws !! 1
              Data.Int.fromString snpu
  mc = case npu of
              Nothing -> Nothing
              Just n -> let
                            mcArr = mapMaybe fromString $ slice 2 (2+n) ws
                        in Just $ MCtruth { pu_zpositions: mcArr }

-- slurps up a String with a bunch of Doubles
-- and parses them w/ to a vertex and a set of helix measurements
hSlurp :: String -> Maybe VHMeas
hSlurp ds = vhm where
  ws = words ds
  npu :: Maybe Int
  npu = do
              key <- head ws
              guard $ key == "PU_zpositions:"
              snpu <- ws !! 1
              Data.Int.fromString snpu
  vhm = case npu of
              Nothing -> hSlurp' $ mapMaybe fromString ws
              Just n -> let vArr = mapMaybe fromString (drop (n+2) ws)
                        in hSlurp' vArr

-- slurp in the measurements of vertex and helices
hSlurp' :: Array Number -> Maybe VHMeas
hSlurp' inp = do
  let v0    = fromList 3 $ take 3 inp       -- initial vertex pos
      cv0   = fromList2 3 3 (take 9 $ drop 3 inp) -- cov matrix
      v     = XMeas v0 cv0
  w2pt      <- inp !! 12  -- how to calc pt from w; 1 in case of CMS
  mnt       <- inp !! 13  -- number of helices to follow --}
  let nt    = Data.Int.round mnt
      f     = case w2pt of
                  1.0 -> nxtH'        -- CMS case
                  otherwise -> nxtH   -- Aleph case
      hl    = mapMaybe (\i -> f w2pt (slice (i*30+14) (i*30+44) inp)) $ range 0 (nt-1)

  pure $ VHMeas { vertex: v, helices: hl }

-- get the next helix, Aleph case
nxtH :: Number -> Array Number -> Maybe HMeas
nxtH w0 ds = do
  let ih    = take 5 ds
      ich   = take 25 $ drop 5 ds
      h'    = fromList 5 ih
      ch'   = fromList2 5 5 ich
  pure $ HMeas (V5 h') (M5 ch') w0

-- get the next helix, CMS case
nxtH' :: Number -> Array Number -> Maybe HMeas
nxtH' _ ds = do
  -- FV works in terms of a perigee system
  -- w = omega = 1/R is curvature radius
  -- tl = tan lambda = tangent of dipping angle (0 for pt-max)
  -- psi = angle in xy plane
  -- d0 = distance of closest approach to 0 in xy plan
  -- z0 = positiion on z axis
  --
  -- CMS uses instead
  -- q/p = charge over momentum
  -- theta = dip angle
  -- etc
  --
  h0 <- ds !! 0
  h1 <- ds !! 1
  h2 <- ds !! 2
  h3 <- ds !! 3
  h4 <- ds !! 4
  let w0                = 0.003*3.8  -- CMS case: field is 3.8 T, give R in cm
      st                = sin h1
      ct                = cos h1
      w                 = h0 * w0 / ct
      tl                = st / ct
      j00               = w0 / ct
      j01               = h0 * w0 * st/ct/ct
      j11               = 1.0 / ct / ct
      j10               = 0.0
      jj                = fromList2 5 5 [  j00, j01, 0.0, 0.0, 0.0
                                          , j10, j11, 0.0, 0.0, 0.0
                                          , 0.0, 0.0, 1.0, 0.0, 0.0
                                          , 0.0, 0.0, 0.0, 1.0, 0.0
                                          , 0.0, 0.0, 0.0, 0.0, 1.0 ]
      h'                = fromList 5 [w, tl, h2, h3, h4]

  let ich               = take 25 $ drop 5 ds
      ch'               = fromList2 5 5 ich
      ch''              = sw (jj) (ch')

  pure $ HMeas (V5 (h')) (M5 ch'') w0

-- slurp all files named in a list of pathNames
{-- hSlurpAll :: forall eff. --}
{--              Array FilePath --}
{--              -> Eff (fs :: FS, exception :: EXCEPTION | eff) --}
{--              (Maybe VHMeas) --}
{-- hSlurpAll [] = do pure Nothing --}
{-- hSlurpAll [p0] = do --}
{--   t0 <- hSlurp p0 --}
{--   let v0 = fst t0 --}
{--   pure v0 --}
{-- hSlurpAll ps = do --}
{--   t0 <- hSlurp $ unsafePartial head ps --}
{--   let v0 = fst t0 --}
{--   vs <- hSlurpAll $ unsafePartial tail ps --}
{--   pure $ v0 -- semigroup not yet implemented... <> vs --}
