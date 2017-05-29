module Test.Cov (testCov) where

import Prelude
import Data.Cov
  ( Cov3, Cov4, Cov5, Jac53, Vec3, Vec5
  , fromArray, inv, toMatrix, tr, chol, cholInv
  , (*.), (|*|), (|.|), (||*||), (|||), (||||), (||*), (**), testCov2
  )
import Data.SimpleMatrix as M

newtype MD = MakeMD {m3 :: Cov3, m5 :: Cov5}
instance showMD :: Show MD where
  show (MakeMD {m3, m5}) = "Show MD,\nm3=" <> show m3 <> "\nm5=" <> show m5

testCov :: Int -> String
testCov cnt = "testCov: " <> show cnt
        {-- <> show md <> "\n" --}
        {-- <> show mm3 --}
        {-- <> show mm5 --}
        {-- <> "exp v3 " <> show ( (v3 + v3) |.| v3 ) <> "\n" --}
        {-- <> show (j53) --}
        {-- <> show (tr j53) --}
        {-- <> "tj3 " <> show tj3 --}
        {-- <> "vv3 " <> show vv3 --}
        {-- <> show (v3 |*| c3) --}
        {-- <> "\n(tr j53 ||*|| c3)" <> show (tr j53 ||*|| c3) --}
        {-- <> "(tr j53 ||| v5)" <> show (tr j53 ||| v5) --}
        {-- <> show (c3 ** (inv c3)) --}
        {-- <> show (c4 ** (inv c4)) --}
        <> "chol: -----------------\n"
        <> "A = L * L^T         " <> show ch3
        <> "L                   " <> show (chol ch3)
        <> "L * L^T             " <> show ((chol ch3) |||| tr (chol ch3))
        <> "A^(-1) = L' * L'^T  " <> show (inv ch3)
        <> "A^(-1) from cholInv " <> show (cholInv ch3 3)
        <> "A = L * L^T         " <> show ch5
        <> "L                   " <> show (chol ch5)
        <> "L * L^T             " <> show ((chol ch5) |||| tr (chol ch5))
        <> "A^(-1) = L' * L'^T  " <> show (inv ch5)
        <> "A^(-1) from cholInv " <> show (cholInv ch5 5)
        {-- <> "chol" <> show ch5 --}
        {-- <> show (chol ch5) <> show ( chol ch5 |||| tr (chol ch5)) --}
        <> testCov2
        where
  c3 :: Cov3
  c3 = fromArray [1.0,2.0,3.0,4.0,5.0,6.0]
  c4 :: Cov4
  c4 = fromArray [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0]
  c5 :: Cov5
  c5 = fromArray [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0]
  c50 :: Cov5
  c50 = fromArray [15.0,14.0,13.0,12.0,11.0,10.0,9.0,8.0,7.0,6.0,5.0,4.0,3.0,2.0,1.0]
  c50m :: Cov5
  c50m = fromArray $ M.toArray $ toMatrix c50
  c51 :: Cov5
  c51 = one
  v3 :: Vec3
  v3 = fromArray [10.0,11.0,12.0]
  v5 :: Vec5
  v5 = fromArray [10.0,11.0,12.0,13.0,14.0]
  j53 :: Jac53
  j53 = fromArray [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0]
  tj3 :: Cov3
  tj3 = j53 ||*|| c5
  vv5 :: Vec5
  vv5 = j53 ||| v3
  vv3 :: Vec3
  vv3 = tr j53 ||| j53 ||| c3 *. v3

  m3 :: M.Matrix
  m3 = M.fromArray2 3 3 [1.0,2.0,3.0,2.0,4.0,5.0,3.0,5.0,6.0]
  mm3 = (m3+m3)*m3
  m5 :: M.Matrix
  m5 = M.fromArray2 5 5 [1.0,2.0,3.0,4.0,5.0, 2.0,6.0,7.0,8.0,9.0
                        ,3.0,7.0,10.0,11.0,12.0, 4.0,8.0,11.0,13.0,14.0
                        ,5.0,9.0,12.0,14.0,15.0]
  mm5 = (m5+m5)*m5
  md = MakeMD {m3: (c3+c3), m5: (c5+c5)}

  ch3 :: Cov3
  ch3 = fromArray [2.0, -1.0, 0.0, 2.0, -1.0, 2.0]
  cch3 = chol ch3
  ich3 = cholInv ch3

  ch5 :: Cov5
  ch5 = fromArray [2.0, -1.0, 0.0, 0.0, 0.0, 2.0, -1.0, 0.0, 0.0, 2.0, 0.0, 0.0, 2.0, 0.0, 2.0]

-----------------------------------------------

