module FV.Fit where

import Prelude
import Data.Array ( foldl, unzip, mapMaybe, length )
import Data.Array.Partial ( head )
import Data.Maybe ( Maybe (..) )
import Data.Tuple ( Tuple (..), fst, snd )
import Math ( abs )
import Partial.Unsafe ( unsafePartial )

import Stuff
import Data.Cov
import FV.Jacob as J
import FV.Types ( VHMeas (..), HMeas (..), QMeas (..), XMeas (..)
                , Prong (..), Chi2 (..)
                , helices
                )

fit :: VHMeas -> Prong
fit vhm = kSmooth vhm <<< kFilter $ vhm

kFilter :: VHMeas -> XMeas
kFilter (VHMeas {vertex, helices}) = foldl kAdd vertex helices

kAdd :: XMeas -> HMeas -> XMeas
kAdd (XMeas v vv) (HMeas h hh w0) = kAdd' x_km1 p_k x_e q_e 1e6 0 where
  x_km1 = XMeas v (inv vv)
  p_k   = HMeas h (inv hh) w0
  x_e   = v
  q_e   = J.hv2q h v

goodEnough :: Number -> Number -> Int -> Boolean
--goodEnough c0 c i | i < 99 && trace ("." <> show i <> "|" <> to1fix (abs (c-c0)) <> " " <> to1fix c) false = undefined
goodEnough c0 c i = abs (c - c0) < chi2cut || i > iterMax where
  chi2cut = 0.5
  iterMax = 99 :: Int

-- | add a helix measurement to kalman filter, return updated vertex position
-- | if we can't invert, don't update vertex
kAdd' :: XMeas -> HMeas -> Vec3 -> Vec3 -> Number -> Int -> XMeas
--kAdd' (XMeas v0 uu0) (HMeas h gg w0) x_e q_e _ i |
--        i == 0 && trace ("kadd'-->" <> show i <> "|" <> show v0 <> show h) false = undefined
kAdd' (XMeas v0 uu0) (HMeas h gg w0) x_e q_e 𝜒2_0 iter = x_k where
  jj    = J.expand x_e q_e
  aa    = jj.aa
  bb    = jj.bb
  h0    = jj.h0
  aaT   = tr aa
  bbT   = tr bb
  x_k   = case invMaybe (bb ||*|| gg) of
            Nothing  -> XMeas v0 (inv uu0) `debug` "... can't invert in kAdd'"
            Just ww  -> let
                gb    = gg - gg *** (bbT ||*|| ww)
                uu    = uu0 + aa ||*|| gb
                cc    = inv uu
                m     = h - h0
                v     = cc *| (uu0 *| v0 + aaT ||| gb *| m)
                dm    = m - aa ||| v
                q     = ww *| bbT ||| gg *| dm
                𝜒2    = (dm - bb ||| q) |*| gg + (v - v0) |*| uu0
                x_k'  = if goodEnough 𝜒2_0 𝜒2 iter -- `debug` ("--> kAdd' chi2 is " <> show 𝜒2)
                  then XMeas v cc
                  else kAdd' (XMeas v0 uu0) (HMeas h gg w0) v q 𝜒2 (iter+1)
              in x_k'

kSmooth :: VHMeas -> XMeas -> Prong
--kSmooth vm v | trace ("kSmooth " <> (show <<< length <<< helices $ vm) <> ", vertex at " <> (show v) ) false = undefined
kSmooth (VHMeas {vertex: v0, helices: hl}) v = pr' where
  (Tuple ql chi2l) = unzip $ mapMaybe (ksm v) hl
  hl' = hl
  n = length hl
  n' = length ql
  n'' = if n == n' then n else n' `debug` "kSmooth killed helices"
  pr' = Prong { fitVertex: v, fitMomenta: ql, fitChi2s: chi2l, nProng: n'', measurements: VHMeas {vertex: v0, helices: hl'} }

-- kalman smoother step: calculate 3-mom q and chi2 at kalman filter'ed vertex
-- if we can't invert, return Nothing and this track will not be included
ksm :: XMeas -> HMeas -> Maybe (Tuple QMeas Chi2)
ksm (XMeas x cc) (HMeas h hh w0) = do
  let
      {-- HMeas (V5 h) (M5 hh) w0 = hm --}
      jj = J.expand x (J.hv2q h x)
      aa = jj.aa
      bb = jj.bb
      h0 = jj.h0
      gg   = inv hh
  ww <- invMaybe (bb ||*|| gg)
  let p    = h - h0
      uu   = inv cc
      aaT  = tr aa
      bbT  = tr bb
      dp   = (p - aa ||| x)
      q    = ww *| bbT ||| gg *| dp
      mee   = (cc *|| aaT) |||| gg *|| bb ||* ww
      dd   = ww + mee ||*|| uu
      r    = p - aa ||| x - bb ||| q
      ch   = r |*| gg
      gb   = gg - gg *** (bbT ||*|| ww)
      uu'  = uu - aa ||*|| gb
      duu  = det uu'
      xxx = "-------->>>>>>>>"
              <> show (uu')
              <> show duu
      bad  = duu < 0.0
      cx   = if det uu' < 0.0 then 1000.0
                                `debug` ("--> ksm bad " <> show (det uu')
                                                        <> show uu')
                    else cx'' where
                      cc'  = inv uu' -- `debug` ("--> ksm " ++ show uu')
                      x'   = cc' *| (uu *| x - aaT ||| gb *| p)
                      dx   = x - x'
                      cx'  = dx |*| uu'
                      cx'' = if cx' < 0.0 then 2000.0 `debug` ("--> ksm chi2 is " <> show cx' <> ", " <> show ch <> ", " <> show ((max cx' 0.0) + ch))
                                        else cx'
      𝜒2 = cx + ch
  pure (Tuple (QMeas q dd w0) (Chi2 𝜒2))

