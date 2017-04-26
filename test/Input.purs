
module Test.Input ( hSlurp, hSlurp' ) where

import Prelude (bind, discard, pure, ($), (==), (+), (-), (*), (/), (<<<), (<>), map, otherwise)
import Control.MonadZero (guard)
import Data.Foldable (foldl)
import Data.Number (fromString) as Data.Number
import Data.Int (fromString, round) as Data.Int
import Data.Array (mapMaybe, head, take, drop, slice, null, (!!), (..))
import Data.Array.Partial (tail)
import Data.Tuple (Tuple (..), fst)
import Data.Maybe (Maybe (..), fromMaybe, fromJust)
import Control.Monad.Aff (Aff)

import Math ( sin, cos )

import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)
import Data.String.Utils (words)
import Unsafe.Coerce (unsafeCoerce)
import Partial.Unsafe (unsafePartial)

import Matrix (sw, fromArray, M (..), V5 (..), M5 (..))
import Types ( MCtruth (..), VHMeas (..), XMeas (..), HMeas (..) )

-- slurp in the measurements of vertex and helices
-- from a list of Doubles
hSlurp' :: Array Number -> Maybe VHMeas
hSlurp' inp = do
  v0        <- fromArray 1 3 $ take 3 inp       -- initial vertex pos
  cv0       <- fromArray 3 3 (take 9 $ drop 3 inp) -- cov matrix
  let v     = XMeas (M v0) (M cv0)
  w2pt      <- inp !! 12                 -- how to calc pt from w; 1 in case of CMS
  mnt       <- inp !! 13  -- number of helices to follow --}
  let nt    = Data.Int.round mnt
      f     = case w2pt of
                  1.0 -> nxtH'        -- CMS case
                  otherwise -> nxtH   -- Aleph case
      hl    = mapMaybe (\i -> f w2pt (slice (i*30+14) (i*30+44) inp)) $ 0..(nt-1)

  pure $ VHMeas { vertex: v, helices: hl }

-- get the next helix, Aleph case
nxtH :: Number -> Array Number -> Maybe HMeas
nxtH w0 ds = do
  let ih = take 5 ds
      ich = take 25 $ drop 5 ds
  h' <- fromArray 1 5 ih
  ch' <- fromArray 5 5 ich
  pure $ HMeas (V5 (M h')) (M5 (M ch')) w0

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
  jj                    <- fromArray 5 5 [  j00, j01, 0.0, 0.0, 0.0
                                            , j10, j11, 0.0, 0.0, 0.0
                                            , 0.0, 0.0, 1.0, 0.0, 0.0
                                            , 0.0, 0.0, 0.0, 1.0, 0.0
                                            , 0.0, 0.0, 0.0, 0.0, 1.0 ]
  h'                    <- fromArray 1 5 [w, tl, h2, h3, h4]

  let ich = take 25 $ drop 5 ds
  ch'                   <- fromArray 5 5 ich
  let ch''              = sw (M jj) (M ch')

  pure $ HMeas (V5 (M h')) (M5 ch'') w0

-- slurps up a bunch of Doubles from a text data file into a list
-- and parses them w/ hSlurp' to a vertex and a set of helix measurements
hSlurp :: forall eff. FilePath
                      -> Aff (fs :: FS | eff)
                             (Tuple (Maybe VHMeas) (Maybe MCtruth))
hSlurp path = do
  ds <- readTextFile UTF8 path
  let ws = words ds
  let npu :: Maybe Int
      npu = do
              key <- head ws
              guard $ key == "PU_zpositions:"
              snpu <- ws !! 1
              Data.Int.fromString snpu
  let mc = case npu of
              Nothing -> Nothing
              Just n -> let
                          mcArr = mapMaybe Data.Number.fromString
                                           (slice 2 (2+n) ws)
                          in Just $ MCtruth { pu_zpositions: mcArr }
      vhm = case npu of
              Nothing -> hSlurp' $ mapMaybe Data.Number.fromString ws
              Just n -> hSlurp' $ mapMaybe Data.Number.fromString (drop (n+2) ws)

  pure $ Tuple vhm mc

-- slurp all files named in a list of pathNames
hSlurpAll :: forall eff.
             Array FilePath
            ->  Aff (fs :: FS | eff)
                    (Maybe VHMeas)
hSlurpAll ps | null ps = do pure Nothing
             | otherwise = do
  p0 <- unsafePartial head ps
  t0 <- hSlurp p0
  v0 <- fst t0
  let p1s = unsafePartial tail ps
      hsa0 :: forall eff. FilePath -> Aff (fs :: FS | eff) (Maybe VHMeas)
      hsa0 acc p = do
        t <- hSlurp p
        v <- fst t
        pure v
  let vhms = foldl hsa0 v0 p1s
  pure vhms
