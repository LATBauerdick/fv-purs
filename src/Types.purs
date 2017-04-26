module Types ( MCtruth (..), VHMeas (..), XMeas (..), HMeas (..)
              ) where

import Prelude
import Data.Monoid
import Data.Array (length)

import Matrix ( M (..), Matrix (..), M5, V5 )

data VHMeas = VHMeas {
    vertex      :: XMeas
  , helices     :: Array HMeas
                     }
{-- instance semigroupHMeas :: Semigroup VHMeas where --}
{--   append (VHMeas v hs) (VHMeas _ hs') = VHMeas v ( hs ++ hs' ) --}
{--   mempty = VHMeas (XMeas (Matrix.zero 3 1) (Matrix.zero 3 3)) [] --}

instance showVHMeas :: Show VHMeas where
  show (VHMeas {vertex, helices}) = "VHMeas w/ " <> show (length helices) <> " tracks."

data MCtruth = MCtruth {
    pu_zpositions :: Array Number
                       }
instance showMCtruth :: Show MCtruth where
  show (MCtruth {pu_zpositions}) = "MCtruth w/" <> show (length pu_zpositions) <> " PU z positions."

data XMeas = XMeas M M -- 3-vector and covariance matrix for position/vertex measurement

instance showXMeas :: Show XMeas where
  show _ = "??? showXMeas to be implemented"

data HMeas = HMeas V5 M5 Number -- 5-vector and covariance matrix for helix measurement

instance showHMeas :: Show HMeas where
  show (HMeas h _ w0) = "{showHMeas: " <> show h <> ", w0=" <> show w0 <> "}"
