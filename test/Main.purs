module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)

import Data.Tuple ( Tuple(..) )
import Data.Array ( (!!) )
import Data.Maybe (Maybe (..) )
import Data.List ( List(..),  (:) )

import Types (typeShow)
import Stuff ( foldr1 )
import Test.Input ( hSlurp )
import Matrix ( Matrix, identity, zero_, matrix, fromList2, getElem, diagonal 
              , multStd
              , toLists, fromLists, fromList
              , nrows, ncols
              )

main :: forall e.  Eff ( exception :: EXCEPTION
                       , console :: CONSOLE
                       , fs :: FS | e
                       ) Unit
main = void $ launchAff do
  log "FVT Test Suite"

  log "Test hSlurp dat/tr05129e001412.dat"
  logShow =<< hSlurp "dat/tr05129e001412.dat"
  log "Test hSlurp dat/tav-4.dat"
  logShow =<< hSlurp "dat/tav-4.dat"
  Tuple vhm _ <- hSlurp "dat/tr05129e001412.dat"
  logShow vhm
  log $ "Test identity 3"
  logShow $ identity 3
  log $ "Test zero 3 3"
  logShow $ zero_ 3 3
  log "Test Matrix operations: identity == zero?"
  logShow $ (identity 3) == (zero_ 3 3)
-- >                                  (  1  0 -1 -2 )
-- >                                  (  3  2  1  0 )
-- >                                  (  5  4  3  2 )
-- > matrix 4 4 $ \(i,j) -> 2*i - j = (  7  6  5  4 )
  log $ "Test matrix creation"
  let m0 = matrix 3 3 $ \(Tuple i j) -> 2*i - j
  logShow $ Tuple (m0 == (fromList2 3 3 [1,0,-1,3,2,1,5,4,3])) m0
  let m1 = matrix 3 4 $ \(Tuple i j) -> 2*i - j
  logShow $ Tuple (m1 == (fromList2 3 3 [1,0,-1,3,2,1,5,4,3])) m1

  log $ "Test getElem"
  let e1 = getElem 3 4 m1
  logShow $ Tuple (e1 == 2) e1

  {-- log $ "Test diagonal" --}
  {-- let d0 = diagonal 0 [1,2,3] --}
  {--     d1 = fromList2 3 3 [1,0,0,0,2,0,0,0,3] --}
  {-- logShow $ Tuple (d0 == d1) d0 --}

  log $ "Test fromLists"
  let ll0 :: List (Array Int)
      ll0 = [1,0,0] : [0,2,0] : [0,0,3] : Nil
      ll1 = fromLists ll0
  logShow $ Tuple (ll1 == diagonal 0 [1,2,3]) ll0

  log $ "Test toLists"
  let l0 = diagonal 0 [1,2,3]
      l1 = toLists l0 !! 2
  logShow $ Tuple (l1 == Just [0,0,3]) l1

  log $ "Test arithmetic"
  let m35 = fromLists $ [1,2,3,4,5] : [2,3,4,5,6] : [3,4,5,6,7] : Nil
      m53 = fromLists $ [1,0,0] : [0,1,0] : [0,0,1] : [0,0,0] : [0,0,0] : Nil
      d5n = diagonal 0.0 [2.0, 2.0, 2.0, 2.0, 2.0]
      d5 = diagonal 0 [2, 2, 2, 2, 2]
  logShow $ m35 * m53 == (fromLists $ [1,2,3] : [2,3,4] : [3,4,5] : Nil)
  logShow $ m35 * m53
  logShow $ (diagonal 0 [1,1,1,1,1]) + (diagonal 0 [1,1,1,1,1]) == (diagonal 0 [2,2,2,2,2])
  logShow $ (fromList2 3 3 [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0]) * one
  logShow $ (fromList2 3 3 [1,2,3,4,5,6,7,8,9]) * one
  logShow $ (fromList2 3 3 [0,0,1,0,1,0,1,0,0]) * (fromList2 3 3 [11, 12,13,21,22,23,31,32,33]) * one
  logShow $ (fromList2 3 3 [11, 12,13,21,22,23,31,32,33]) * fromList 3 [-1,1,1]

  log "Test typeShow"
  log $ typeShow
  {-- (VHMeas _ hl, _) <- hSlurp "dat/tav-0.dat" --}
  {-- let HMeas _ _ w = head hl --}
  {-- w `shouldBe` 0.0114 --}

