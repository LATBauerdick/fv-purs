module Test.Matrix ( testMatrix ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Tuple (Tuple (..) )
import Data.Maybe
import Data.Array ( (!!), length, zip, range, index, foldl )
import Data.List ( List(..),  (:), mapMaybe )

import Data.Matrix
  ( Matrix
  , identity, zero_, matrix, fromArray2
  , getElem, diagonal, subm2, submatrix
  , multStd
  , toLists, fromArrays, fromArray
  , nrows, ncols
  )


testMatrix :: forall e. Eff (console :: CONSOLE | e) Unit
testMatrix = do
  log $ "Test identity 3"
  logShow $ identity 3
  log $ "Test zero 3 3"
  logShow $ zero_ 3 3
  log "Test Matrix operations: identity == zero?"
  logShow $ (identity 3) == (zero_ 3 3)
  log $ "Test matrix creation"
  let m0 = matrix 3 3 $ \(Tuple i j) -> 2*i - j
  logShow $ Tuple (m0 == (fromArray2 3 3 [1,0,-1,3,2,1,5,4,3])) m0
  let m1 = matrix 3 4 $ \(Tuple i j) -> 2*i - j
  logShow $ Tuple (m1 == (fromArray2 3 3 [1,0,-1,3,2,1,5,4,3])) m1

  log $ "Test getElem"
  let e1 = getElem 3 4 m1
  logShow $ Tuple (e1 == 2) e1

  log $ "Test fromArrays"
  let ll0 :: List (Array Int)
      ll0 = [1,0,0] : [0,2,0] : [0,0,3] : Nil
      ll1 = fromArrays ll0
  logShow $ Tuple (ll1 == diagonal 0 [1,2,3]) ll0

  log $ "Test toLists"
  let l0 = diagonal 0 [1,2,3]
      l1 = toLists l0 !! 2
  logShow $ Tuple (l1 == Just [0,0,3]) l1

  log $ "Test arithmetic"
  let m35 = fromArrays $ [1,2,3,4,5] : [2,3,4,5,6] : [3,4,5,6,7] : Nil
      m53 = fromArrays $ [1,0,0] : [0,1,0] : [0,0,1] : [0,0,0] : [0,0,0] : Nil
      d5n = diagonal 0.0 [2.0, 2.0, 2.0, 2.0, 2.0]
      d5 = diagonal 0 [2, 2, 2, 2, 2]
  logShow $ m35 * m53 == (fromArrays $ [1,2,3] : [2,3,4] : [3,4,5] : Nil)
  logShow $ m35 * m53
  logShow $ (diagonal 0 [1,1,1,1,1]) + (diagonal 0 [1,1,1,1,1]) == (diagonal 0 [2,2,2,2,2])
  logShow $ (fromArray2 3 3 [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0]) * one
  logShow $ (fromArray2 3 3 [1,2,3,4,5,6,7,8,9]) * one
  logShow $ (fromArray2 3 3 [0,0,1,0,1,0,1,0,0]) * (fromArray2 3 3 [11, 12,13,21,22,23,31,32,33]) * one
  logShow $ (fromArray2 3 3 [11, 12,13,21,22,23,31,32,33]) * fromArray 3 [-1,1,1]

  {-- log "Test submatrix" --}
  {-- let xxms = fromArray2 5 5 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25] --}
  {-- logShow $ submatrix 2 3 1 3 $ subm2 4 xxms --}
