module FV.Fit where

import Prelude
import Data.Array ( foldl, unzip, mapMaybe, length )
import Data.Maybe ( Maybe (..) )
import Data.Tuple ( Tuple (..), fst, snd )
import Math ( abs )

import Stuff
--import Data.Matrix ( sw, tr, scalar, inv, invMaybe, det )
import Data.Cov
--import FV.Coeff ( expand, hv2q ) as Coeff
import FV.Jacob as J
{-- import FV.Types ( VHMeas (..), HMeas (..), QMeas (..), XMeas (..) --}
{--                 , Prong (..), Chi2 --}
{--                 , helices --}
{--                 ) --}
data VHMeas = VHMeas { vertex :: XMeas, helices :: Array HMeas }
helices (VHMeas {helices}) = helices
data XMeas = XMeas Vec3 Cov3
data QMeas = QMeas Vec3 Cov3
data HMeas = HMeas Vec5 Cov5 Number
newtype Chi2 = Chi2 Number
data Prong = Prong { }

fit :: VHMeas -> Prong
fit v = Prong {}--  undefined
{-- fit vhm = kSmooth vhm <<< kFilter $ vhm --}

{-- kFilter :: VHMeas -> XMeas --}
{-- kFilter (VHMeas {vertex, helices}) = foldl kAdd vertex helices --}

{-- kAdd :: XMeas -> HMeas -> XMeas --}
{-- kAdd (XMeas v vv) (HMeas h hh w0) = kAdd' x_km1 p_k x_e q_e 1e6 0 where --}
{--   x_km1 = XMeas v (inv vv) --}
{--   p_k   = HMeas h (inv hh) w0 --}
{--   x_e   = v --}
{--   q_e   = J.hv2q h v --}

{-- goodEnough :: Number -> Number -> Int -> Boolean --}
{-- goodEnough c0 c i | i < 3 && trace ("." <> show i <> "|" <> to1fix (abs (c-c0)) <> to1fix c) false = undefined --}
{-- goodEnough c0 c i = abs (c - c0) < chi2cut || i > iterMax where --}
{--   chi2cut = 0.5 --}
{--   iterMax = 99 :: Int --}

{-- -- | add a helix measurement to kalman filter, return updated vertex position --}
{-- -- | if we can't invert, don't update vertex --}
{-- kAdd' :: XMeas -> HMeas -> Vec3 -> Vec3 -> Number -> Int -> XMeas --}
{-- kAdd' (XMeas v0 uu0) (HMeas h gg w0) x_e q_e 𝜒2_0 iter = x_k where --}
{--   jj    = J.expand x_e q_e --}
{--   aa    = jj.aa --}
{--   bb    = jj.bb --}
{--   h0    = jj.h0 --}
{--   aaT   = tr aa --}
{--   bbT   = tr bb --}
{--   x_k   = case invMaybe (sw3 bb gg) of --}
{--             Nothing  -> XMeas v0 (inv uu0) -- `debug` "... in kAdd'" --}
{--             Just ww  -> let --}
{--                 gb    = gg - gg * (sw5 bbT ww) * gg --}
{--                 uu    = uu0 + sw3 aa gb --}
{--                 cc    = inv uu --}
{--                 m     = h - h0 --}
{--                 v     = cc * (uu0 * v0 + aaT * gb * m) --}
{--                 dm    = m - aa * v --}
{--                 q     = ww * bbT * gg * dm --}
{--                 𝜒2    = sw1 (dm - bb * q) gg + sw1 (v - v0) uu0 --}
{--                 x_k'  = if goodEnough 𝜒2_0 𝜒2 iter -- `debug` ("--> kAdd' chi2 is " ++ show 𝜒2) --}
{--                   then XMeas v cc --}
{--                   else kAdd' (XMeas v0 uu0) (HMeas h gg w0) v q 𝜒2 (iter+1) --}
{--               in x_k' --}

{-- kSmooth :: VHMeas -> XMeas -> Prong --}
{-- kSmooth vm v | trace ("kSmooth " <> (show <<< length <<< helices $ vm) <> ", vertex at " <> (show v) ) false = undefined --}
{-- kSmooth (VHMeas {vertex: v0, helices: hl}) v = pr' where --}
{--   {1-- (Tuple ql chi2l) = unzip $ mapMaybe (ksm v) hl --1} --}
{--   xx = unzip $ mapMaybe (ksm v) hl --}
{--   ql = fst xx --}
{--   chi2l = snd xx --}
{--   hl' = hl --}
{--   n = length hl --}
{--   n' = length ql --}
{--   n'' = if n == n' then n else n' `debug` "kSmooth killed helices" --}
{--   pr' = Prong { fitVertex: v, fitMomenta: ql, fitChi2s: chi2l, nProng: n'', measurements: VHMeas {vertex: v0, helices: hl'} } --}

{-- -- kalman smoother step: calculate 3-mom q and chi2 at kalman filter'ed vertex --}
{-- -- if we can't invert, return Nothing and this track will not be included --}
{-- ksm :: XMeas -> HMeas -> Maybe (Tuple QMeas Chi2) --}
{-- ksm (XMeas x cc) (HMeas h hh w0) = do --}
{--   let --}
{--       {1-- HMeas (V5 h) (M5 hh) w0 = hm --1} --}
{--       jj = J.expand x (J.hv2q h x) --}
{--       aa = jj.aa --}
{--       bb = jj.bb --}
{--       h0 = jj.h0 --}
{--       gg   = inv hh --}
{--   ww <- invMaybe (sw3 bb gg) --}
{--   let --}
{--       p    = h - h0 --}
{--       uu   = inv cc --}
{--       aaT  = tr aa --}
{--       bbT  = tr bb --}
{--       q    = ww *| bbT *| gg *| (p - (aa *| x)) --}
{--       ee   = - cc * aaT * gg * bb * ww --}
{--       dd   = ww + sw3 ee uu --}
{--       r    = p - aa*x - bb*q --}
{--       ch   = sw1 r gg --}

{--       gb   = gg - gg * (sw5 bbT ww) * gg --}
{--       uu'  =  uu - sw3 aa gb --}
{--       duu  = det uu' --}
{--       bad  = duu < 0.0 --}
{--       cx   = if bad then 1000.0 `debug` ("--> ksm bad" <> show duu <> show uu') --}
{--                     else cx'' where --}
{--                       cc'  = inv uu' -- `debug` ("--> ksm " ++ show uu') --}
{--                       x'   = cc' * (uu*x - aaT * gb * p) --}
{--                       dx   = x - x' --}
{--                       cx'  = sw1 dx uu' --}
{--                       cx'' = if cx' < 0.0 then 2000.0 `debug` ("--> ksm chi2 is " <> show cx' <> ", " <> show ch <> ", " <> show ((max cx' 0.0) + ch)) --}
{--                                         else cx' --}
{--       chi2 = cx + ch --}
{--   pure (Tuple (QMeas q dd w0) chi2) --}

