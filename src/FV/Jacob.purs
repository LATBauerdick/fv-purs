module FV.Jacob
  ( expand
  , hv2q
  ) where

import Prelude
import Prelude.Extended (mod', uidx, debug)

import Math ( sqrt, atan2, cos, sin, atan, pi )

import Data.Cov (Jacs, Vec3, Vec5, fromArray, val, Jac (..) )

-- | calculate q 3-vector for a given helix parameterization near vertex position
hv2q :: Vec5 -> Vec3 -> Vec3
hv2q h v = q where
  v_   = val v
  xx   = uidx v_ 0
  yy   = uidx v_ 1
  r    = sqrt $ xx*xx + yy*yy
  phi  = atan2 yy xx
  h_   = val h
  w0   = uidx h_ 0
  tl0  = uidx h_ 1
  psi0 = uidx h_ 2
  d0   = uidx h_ 3
  z0   = uidx h_ 4
  xi = mod' (psi0 - phi + 2.0*pi) (2.0*pi)
  cxi = cos xi
  sxi = sin xi
  q = fromArray $
            if w0 /= 0.0 
                then [ w0, tl0, psi0 + gamma ]
                else [ w0, tl0, psi0 ]
                  where
                    oow0 = 1.0/w0
                    gamma = atan r*cxi/(oow0-r*sxi)

expand :: Vec3 -> Vec3 -> Jacs
expand v q = {aa: aa, bb: bb, h0: h0} where
  v_  = val v
  xx  = uidx v_ 0
  yy  = uidx v_ 1
  z   = uidx v_ 2
  r   = sqrt $ xx*xx + yy*yy
  phi = atan2 yy xx
  q_  = val q
  w   = uidx q_ 0
  tl  = uidx q_ 1
  psi = uidx q_ 2
  -- some more derived quantities
  xi  = mod' (psi - phi + 2.0*pi) (2.0*pi)
  cxi = cos xi
  sxi = sin xi
  oow = 1.0 / w
  rw  = r * w

  gamma = atan $ r*cxi/(oow - r*sxi)
  sg    = sin gamma
  cg    = cos gamma

  -- calculate transformed quantities
  psi0  = psi - gamma
  d0    = oow - (oow - r*sxi)/cg
  z0    = z - tl*gamma/w

  -- calc Jacobian
  drdx    =    if r /= 0.0 then xx/r else 0.0
  drdy    =    if r /= 0.0 then yy/r else 0.0
  rdxidx  =    if r /= 0.0 then yy/r else 0.0
  rdxidy  =    if r /= 0.0 then -xx/r else 0.0
  dgdvar0 =    1.0/(1.0 + rw*rw - 2.0*rw*sxi)
  dgdx    =    dgdvar0*(w*cxi*drdx + w*(rw - sxi)*rdxidx)
  dgdy    =    dgdvar0*(w*cxi*drdy + w*(rw - sxi)*rdxidy)
  dgdw    =    dgdvar0*r*cxi
  dgdpsi  =    dgdvar0*rw*(rw - sxi)

  --  fill matrix:
  -- d w / d r, d phi, d z
  a11                = 0.0
  a12                = 0.0
  a13                = 0.0
  -- d tl / d x, d y, d z
  a21                = 0.0
  a22                = 0.0
  a23                = 0.0
  -- d psi0 / d x, d y, d z
  a31                = -dgdx
  a32                = -dgdy
  a33                = 0.0
  -- d d0 / d x, d y, d z
  a41                = cxi*rdxidx/cg + sxi*drdx/cg
                        - (oow - r*sxi)*sg*dgdx/cg/cg
  a42                = cxi*rdxidy/cg + sxi*drdy/cg
                        - (oow - r*sxi)*sg*dgdy/cg/cg
  a43                = 0.0
  -- d z0 / d x, d y, d z
  a51                = -tl/w*dgdx
  a52                = -tl/w*dgdy
  a53                = 1.0

  -- B
  -- d w / d w, d tl, d psi
  b11                = 1.0
  b12                = 0.0
  b13                = 0.0
  -- d tl / d w, d tl, d psi
  b21                = 0.0
  b22                = 1.0
  b23                = 0.0
  -- d psi0 / d w, d tl, d psi
  b31                = -dgdw
  b32                = 0.0
  b33                = 1.0 - dgdpsi
  -- d d0 / d w, d tl, d psi
  b41                =  -oow*oow*(1.0 - 1.0/cg)
                        - (oow - r*sxi)*sg*dgdw/cg/cg
  b42                = 0.0
  b43                = r*cxi/cg - (oow - r*sxi)*sg*dgdpsi/cg/cg
  -- d z0 / d w, d tl, d psi
  b51                = -tl/w*(dgdw - gamma/w)
  b52                = -gamma/w
  b53                = -tl/w*dgdpsi

  v01                = xx
  v02                = yy
  v03                = z
  q01                = w
  q02                = tl
  q03                = psi
  h0                 = fromArray [
      0.0,
      0.0,
      psi0 - a31*v01 - a32*v02 - b31*q01 - b33*q03,
      d0 - a41*v01 - a42*v02 - b41*q01 - b43*q03,
      z0 - a51*v01 - a52*v02 - a53*v03 - b51*q01 - b52*q02 - b53*q03]
  aa = Jac { v: [a11,a12,a13,a21,a22,a23,a31,a32,a33,a41,a42,a43,a51,a52,a53], nr: 5}
  bb = Jac { v: [b11,b12,b13,b21,b22,b23,b31,b32,b33,b41,b42,b43,b51,b52,b53], nr: 5}
  {-- aaT = tr aa `debug` ( "v0 --->> " <> (show v) <> --}
  {--                        "q0 --->> " <> (show q) <> --}
  {--                        "aa --->> " <> show aa <> --}
  {--                        "bb --->> " <> show bb <> --}
  {--                        "h0 --->> " <> (show h0) --}
  {--                        ) --}

