module Types ( MCtruth (..), VHMeas (..), XMeas (..), HMeas (..)
              , M5 (..), V5 (..), typeShow  
              ) where

import Prelude
-- import Data.Monoid
import Data.Array (length)
import Stuff
import Matrix ( M, nrows, ncols, values, fromList2, diagonal, identity
              , Matrix (..), elementwiseUnsafePlus, zero_, multStd )

newtype Dim3 = Dim3 Int
newtype Dim5 = Dim5 Int
newtype MM a = MM (Matrix Number)

instance semiringMM3 :: Semiring (MM Dim3) where
  add (MM m1) (MM m2) = MM $ elementwiseUnsafePlus m1 m2
  zero = MM $ zero_ 3 3
  mul (MM m1) (MM m2) = MM $ multStd m1 m2
  one = MM $ identity 3
instance showMM3 :: Show (MM Dim3) where
  show (MM m) = "Show (MM Dim3) " <> show m 

instance semiringMM5 :: Semiring (MM Dim5) where
  add (MM m1) (MM m2) = MM $ elementwiseUnsafePlus m1 m2
  zero = MM $ zero_ 5 5
  mul (MM m1) (MM m2) = MM $ multStd m1 m2
  one = MM $ identity 5

instance showMM5 :: Show (MM Dim5) where
  show (MM m) = "Show (MM Dim5) " <> show m 

newtype MD = MakeMD {m3 :: (MM Dim3), m5 :: MM Dim5}
instance showMD :: Show MD where
  show (MakeMD {m3, m5}) = "Show MD, m3=" <> show m3 <> "\nm5=" <> show m5  

typeShow = "testShow: " <> show md where
  mm3 :: MM Dim3
  mm3 = MM $ fromList2 3 3 [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0]
  m3 = ((mm3 + mm3)*mm3)
  m5 :: MM Dim5
  m5 = one 
  md = MakeMD {m3, m5}

newtype V5 = V5 M
newtype M5 = M5 M
instance showV5 :: Show V5 where
  show _ = "show V5: T.B.I."

data VHMeas = VHMeas {
    vertex      :: XMeas
  , helices     :: Array HMeas
                     }
{-- instance semigroupHMeas :: Semigroup VHMeas where --}
{--   append (VHMeas v hs) (VHMeas _ hs') = VHMeas v ( hs ++ hs' ) --}
{--   mempty = VHMeas (XMeas (Matrix.zero 3 1) (Matrix.zero 3 3)) [] --}

instance showVHMeas :: Show VHMeas where
  show (VHMeas {vertex, helices}) = "VHMeas w/ " <> show (length helices) <> " tracks. " <> show vertex

data MCtruth = MCtruth {
    pu_zpositions :: Array Number
                       }
instance showMCtruth :: Show MCtruth where
  show (MCtruth {pu_zpositions}) = "MCtruth w/" <> show (length pu_zpositions) <> " PU z positions."

data XMeas = XMeas M M -- 3-vector and covariance matrix for position/vertex measurement

instance showXMeas :: Show XMeas where
  show (XMeas x _) = "XMeas at " <> show (values x)

data HMeas = HMeas V5 M5 Number -- 5-vector and covariance matrix for helix measurement

instance showHMeas :: Show HMeas where
  show (HMeas h _ w0) = "{showHMeas: " <> show h <> ", w0=" <> show w0 <> "}"
