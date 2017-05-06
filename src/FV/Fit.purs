module FV.Fit ( fit ) where

import Prelude
import Data.Array ( foldl, unzip, mapMaybe, length )
import Data.Maybe ( Maybe (..) )
import Data.Tuple ( Tuple (..) )
import Math ( abs )

import Stuff
import Data.Matrix ( sw, tr, scalar, inv, invMaybe, det )
import FV.Coeff ( expand, hv2q ) as Coeff
import FV.Types ( VHMeas (..), HMeas (..), QMeas (..), XMeas (..)
                , Prong (..), Chi2
                , V3 (..), M3 (..), V5 (..), M5 (..), Jaco (..)
                )

fit :: VHMeas -> Prong
fit vhm = kSmooth vhm <<< kFilter $ vhm

kFilter :: VHMeas -> XMeas
kFilter (VHMeas {vertex, helices}) = foldl kAdd vertex helices

kAdd :: XMeas -> HMeas -> XMeas
kAdd (XMeas v (M3 vv)) (HMeas h (M5 hh) w0) = kAdd' x_km1 p_k x_e q_e 1e6 0 where
  x_km1 = XMeas v (M3 (inv vv))
  p_k   = HMeas h (M5 (inv hh)) w0
  x_e   = v
  q_e   = Coeff.hv2q h v

goodEnough :: Number -> Number -> Int -> Boolean
--goodEnough c0 c i | trace ("."++show i ++ "|" ++ printf "%8.1f" (abs (c-c0)) ++ printf "%8.1f" c) False = undefined
goodEnough c0 c i = abs (c - c0) < chi2cut || i > iterMax where
  chi2cut = 0.5
  iterMax = 99 :: Int

-- | add a helix measurement to kalman filter, return updated vertex position
-- | if we can't invert, don't update vertex
kAdd' :: XMeas -> HMeas -> V3 -> V3 -> Number -> Int -> XMeas
kAdd' (XMeas (V3 v0) (M3 uu0)) (HMeas (V5 h) (M5 gg) w0) x_e q_e 𝜒2_0 iter = x_k where
  Jaco aa bb (V5 h0) = Coeff.expand x_e q_e
  aaT   = tr aa
  bbT   = tr bb
  x_k   = case invMaybe (sw bb gg) of
            Nothing  -> (XMeas (V3 v0) (M3 (inv uu0))) -- `debug` "... in kAdd'"
            Just ww' -> let
                ww    = ww'
                gb    = gg - sw gg (sw bbT ww)
                uu    = uu0 + sw aa gb
                cc    = inv uu
                m     = h - h0
                v     = cc * (uu0 * v0 + aaT * gb * m)
                dm    = m - aa * v
                q     = ww * bbT * gg * dm
                𝜒2    = scalar $ sw (dm - bb * q) gg + sw (v - v0) uu0
                x_k'  = if goodEnough 𝜒2_0 𝜒2 iter -- `debug` ("--> kAdd' chi2 is " ++ show 𝜒2)
                  then XMeas (V3 v) (M3 cc)
                  else kAdd' (XMeas (V3 v0) (M3 uu0)) (HMeas (V5 h) (M5 gg) w0) (V3 v) (V3 q) 𝜒2 (iter+1)
              in x_k'

kSmooth :: VHMeas -> XMeas -> Prong
--kSmooth vm v | trace ("kSmooth " ++ (show . length . view helicesLens $ vm) ++ ", vertex at " ++ (show v) ) False = undefined
kSmooth (VHMeas {vertex: v0, helices: hl}) v = pr' where
  (Tuple ql chi2l) = unzip $ mapMaybe (ksm v) hl
  hl' = hl
  (Tuple n n') = (Tuple (length hl) (length ql))
  n'' = if n == n' then n else n' `debug` "kSmooth killed helices"
  pr' = Prong { fitVertex: v, fitMomenta: ql, fitChi2s: chi2l, nProng: n'', measurements: VHMeas {vertex: v0, helices: hl'} }

-- kalman smoother step: calculate 3-mom q and chi2 at kalman filter'ed vertex
-- if we can't invert, return Nothing and this track will not be included
ksm :: XMeas -> HMeas -> Maybe (Tuple QMeas Chi2)
ksm (XMeas (V3 x) (M3 cc)) hm = do
  let
      HMeas (V5 h) (M5 hh) w0 = hm
      Jaco aa bb (V5 h0) = Coeff.expand (V3 x) (Coeff.hv2q (V5 h) (V3 x))
      gg   = inv hh
  ww <- invMaybe (sw bb gg)
  let
      p    = h - h0
      uu   = inv cc
      aaT  = tr aa
      bbT  = tr bb
      q    = ww * bbT * gg * (p - aa * x)
      ee   = - cc * aaT * gg * bb * ww
      dd   = ww + sw ee uu
      r    = p - aa*x - bb*q
      ch   = scalar $ sw r gg

      gb   = gg - sw gg (sw bbT ww)
      uu'  =  uu - sw aa gb
      duu  = det uu'
      bad  = duu < 0.0
      cx   = if bad then 1000.0 `debug` ("--> ksm bad" <> show duu <> show uu')
                    else cx'' where
                      cc'  = inv uu' -- `debug` ("--> ksm " ++ show uu')
                      x'   = cc' * (uu*x - aaT * gb * p)
                      dx   = x - x'
                      cx'  = scalar $ sw dx uu'
                      cx'' = if cx' < 0.0 then 2000.0 `debug` ("--> ksm chi2 is " <> show cx' <> ", " <> show ch <> ", " <> show ((max cx' 0.0) + ch))
                                        else cx'
      chi2 = cx + ch
  pure (Tuple (QMeas q dd w0) chi2)
