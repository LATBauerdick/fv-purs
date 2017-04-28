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
import Data.Foldable ( foldr )

import Stuff ( gcd', foldr1 )
import Test.Input ( hSlurp )
import Matrix ( Matrix, identity, zero, matrix, fromList2, getElem, diagonal 
              , (+.), (-.)
              , toLists, fromLists
              , nrows, ncols
              )

main :: forall e.  Eff ( exception :: EXCEPTION
                       , console :: CONSOLE
                       , fs :: FS | e
                       ) Unit
main = void $ launchAff do
  log "FVT Test Suite"
  logShow $ gcd' 121 22

  log "Test hSlurp dat/tr05129e001412.dat"
  logShow =<< hSlurp "dat/tr05129e001412.dat"
  log "Test hSlurp dat/tav-4.dat"
  logShow =<< hSlurp "dat/tav-4.dat"
  Tuple vhm _ <- hSlurp "dat/tr05129e001412.dat"
  logShow vhm
  log $ "Test identity 3"
  logShow $ identity 3
  log $ "Test zero 3 3"
  logShow $ zero 3 3
  log "Test Matrix operations: identity == zero?"
  logShow $ (identity 3) == (zero 3 3)
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
  log $ "Test diagonal"
  let d0 = diagonal 0 [1,2,3]
      d1 = fromList2 3 3 [1,0,0,0,2,0,0,0,3]
  logShow $ Tuple (d0 == d1) d0
  log $ "Test fromLists"
  let ll0 :: List (Array Int)
      ll0 = [1,0,0] : [0,2,0] : [0,0,3] : Nil
      ll1 = fromLists ll0
  logShow $ Tuple (ll1 == d0) ll0
  log $ "Test toLists"
  let l0 = diagonal 0 [1,2,3]
      l1 = toLists l0 !! 2
  logShow $ Tuple (l1 == Just [0,0,3]) l1
  log $ "Test arithmetic"
  let i5 :: Matrix Number --!!! Int
      i5 = identity 5
      d5n = diagonal 0.0 [2.0, 2.0, 2.0, 2.0, 2.0] --}
      d5 = diagonal 0 [2, 2, 2, 2, 2]
  logShow $ i5 +. i5 == d5n
  logShow $ Tuple ((diagonal 0 [1,1,1,1,1]) +. (diagonal 0 [1,1,1,1,1]) == (diagonal 0 [2,2,2,2,2])) i5


  logShow $ foldr1 (<>) $ [11,12] : [21,22] : Nil
  {-- (VHMeas _ hl, _) <- hSlurp "dat/tav-0.dat" --}
  {-- let HMeas _ _ w = head hl --}
  {-- w `shouldBe` 0.0114 --}

