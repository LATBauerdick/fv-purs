module Matrix (M (..), Matrix (..), M5 (..), V5 (..), sw, fromArray) where

import Prelude
import LinearAlgebra.Matrix (Matrix (..), zeros, fromArray) as LAM

type Matrix = LAM.Matrix Number
newtype M = M (Matrix)
newtype V5 = V5 M
newtype M5 = M5 M

instance showM :: Show M where
  show _ = "show Matrix: T.B.I."

instance showV5 :: Show V5 where
  show _ = "show V5: T.B.I."

fromArray = LAM.fromArray

sw :: M -> M -> M
sw a b = a

