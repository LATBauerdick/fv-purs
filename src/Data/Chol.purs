module Data.Chol (doCholdc, doCholInv)
  where

import Prelude
import Prelude.Extended ( uidx, uJust, error, fromList, range )
import Math ( abs, sqrt )
import Data.Foldable ( sum )

import Data.Array (
    replicate
  , take
  ) as A
import Control.Monad.ST ( ST )
import Control.Monad.ST ( run, for ) as ST
import Data.Array.ST ( STArray )
import Data.Array.ST ( peek, poke, unsafeFreeze, thaw ) as STA

indV :: Int -> Int -> Int -> Int
indV w i0 j0 = (i0*w+j0)
indVs :: Int -> Int -> Int -> Int
indVs w i0 j0 | i0 <= j0   = (i0*w - (i0*(i0-1)) `div` 2 + j0-i0)
              | otherwise = (j0*w - (j0*(j0-1)) `div` 2 + i0-j0)
run :: forall a. (forall r. ST r (STArray r a)) -> Array a
run act = ST.run (act >>= STA.unsafeFreeze)

-- CHOLESKY DECOMPOSITION

-- | Simple Cholesky decomposition of a symmetric, positive definite matrix.
--   The result for a matrix /M/ is a lower triangular matrix /L/ such that:
--
--   * /M = LL^T/.
--
--   Example:
--
-- >            (  2 -1  0 )   (  1.41  0     0    )
-- >            ( -1  2 -1 )   ( -0.70  1.22  0    )
-- > choldx     (  0 -1  2 ) = (  0.00 -0.81  1.15 )
--
-- Given a positive-deﬁnite symmetric matrix a[1..n][1..n],
-- this routine constructs its Cholesky decomposition,
-- A = L · L^T
-- The Cholesky factor L is returned in the lower triangle of a,
-- except for its diagonal elements which are returned in p[1..n].

doCholdc :: Array Number -> Int -> Array Number
doCholdc a n = a' where
  ll = n*n
  l = run (do
    arr <- STA.thaw (A.replicate (ll+n+1) 0.0)
    -- loop over input array using Numerical Recipies algorithm (chapter 2.9)
    let ixa = indVs n
        ixarr = indV n
    ST.for 0 n \i0 -> do
      ST.for i0 n \j0 -> do
        let aij = uidx a (ixa i0 j0)
        _  <- if i0==j0
                    then STA.poke (ll+i0) aij arr
                    else STA.poke (ixarr j0 i0) aij arr
        ST.for 0 (i0+1) \k0 -> do

{-- peekSTArray :: forall a h r. STArray h a -> Int -> Eff (st :: ST h | r) (Maybe a) --}
{-- peek :: forall h a.          Int -> STArray h a -> ST h (Maybe a) --}
  {-- ST.run (do --}
  {--   arr <- STA.thaw [1, 2, 3] --}
  {--   STA.peek 2 arr) == Just 3 --}

          maik <- STA.peek (ixarr i0 k0) arr
          majk <- STA.peek (ixarr j0 k0) arr
          maij <- if i0==j0
                    then STA.peek (ll+i0) arr
                    else STA.peek (ixarr j0 i0) arr
          let sum = (uJust maij) - (uJust maik) * (uJust majk)
          void $ if i0==j0
                    then STA.poke (ll+i0) sum arr
                    else STA.poke (ixarr j0 i0) sum arr
        msum <- if i0==j0
                    then STA.peek (ll+i0) arr
                    else STA.peek (ixarr j0 i0) arr
        let sum = if i0==j0 && (uJust msum) < 0.0
                    then error ("choldc: not a positive definite matrix " <> show a)
                    else (uJust msum)
        mp_i' <- STA.peek (ll+i0) arr
        let p_i = if i0 == j0 then sqrt sum else (uJust mp_i')
        void $ if i0==j0
                    then STA.poke (ll+i0) p_i arr
                    else STA.poke (ixarr j0 i0) (sum/p_i) arr
        pure $ unit

    -- copy diagonal back into array
    ST.for 0 n \i0 -> do
      maii <- STA.peek (ll+i0) arr
      void $ STA.poke (ixarr i0 i0) (uJust maii) arr

    pure arr)
  a' = A.take (n*n) l

-- | Matrix inversion using Cholesky decomposition
-- | based on Numerical Recipies formula in 2.9
--
doCholInv :: Array Number -> Int -> Array Number
doCholInv a n = a' where
  ll = n*n
  l = run (do -- make a STArray of n x n + space for diagonal +1 for summing
    arr <- STA.thaw (A.replicate (ll+n+1) 0.0)
    -- loop over input array using Numerical Recipies algorithm (chapter 2.9)
    let ixa = indVs n
        ixarr = indV n
    ST.for 0 n \i0 -> do
      ST.for i0 n \j0 -> do
        let aij = uidx a (ixa i0 j0)
        void $ if i0==j0
                  then STA.poke (ll+i0) aij arr
                  else STA.poke (ixarr j0 i0) aij arr
        ST.for 0 (i0+1) \k0 -> do
          maik <- STA.peek (ixarr i0 k0) arr
          majk <- STA.peek (ixarr j0 k0) arr
          maij <- if i0==j0
                      then STA.peek (ll+i0) arr
                      else STA.peek (ixarr j0 i0) arr
          let sum = (uJust maij) - (uJust maik) * (uJust majk)
          void $ if i0==j0
                    then STA.poke (ll+i0) sum arr
                    else STA.poke (ixarr j0 i0) sum arr
        msum <- if i0==j0
                    then STA.peek (ll+i0) arr
                    else STA.peek (ixarr j0 i0) arr
        let sum = if i0==j0 && (uJust msum) < 0.0
                    then error ("choldInv: not a positive definite matrix " <> show a)
                    else (uJust msum)
        mp_i' <- STA.peek (ll+i0) arr
        let p_i = if i0 == j0
                      then sqrt sum
                      else (uJust mp_i')
        void $ if i0==j0
                  then STA.poke (ll+i0) p_i arr
                  else STA.poke (ixarr j0 i0) (sum/p_i) arr
        pure $ unit

    -- invert L -> L^(-1)
    ST.for 0 n \i0 -> do
      mp_i <- STA.peek (ll+i0) arr
      void $ STA.poke (ixarr i0 i0) (1.0/(uJust mp_i)) arr
      ST.for (i0+1) n \j0 -> do
        void $ STA.poke (ll+n) 0.0 arr
        ST.for i0 (j0+1) \k0 -> do
          majk <- STA.peek (ixarr j0 k0) arr
          maki <- STA.peek (ixarr k0 i0) arr
          sum <- STA.peek (ll+n) arr
          void $ STA.poke (ll+n) ((uJust sum) - (uJust majk) * (uJust maki)) arr
        msum <- STA.peek (ll+n) arr
        mp_j <- STA.peek (ll+j0) arr
        void $ STA.poke (ixarr j0 i0) ((uJust msum)/(uJust mp_j)) arr
    pure arr)

  a' = fromList $ do
    let idx = indV n
    i0 <- range 0 (n-1)
    j0 <- range i0 (n-1)
    let aij = sum $ do
                  k0 <- range 0 (n-1)
                  pure $ (uidx l (idx k0 i0)) * (uidx l (idx k0 j0))
    pure $ aij

--C version Numerical Recipies 2.9
--for (i=1;i<=n;i++) {
--  for (j=i;j<=n;j++) {
--    for (sum=a[i][j],k=i-1;k>=1;k--) sum -= a[i][k]*a[j][k];
--    if (i == j) {
--      if (sum <= 0.0) nrerror("choldc failed");
--      p[i]=sqrt(sum);
--    } else a[j][i]=sum/p[i];
--  }
--}
-- In this, and many other applications, one often needs L^(−1) . The lower
-- triangle of this matrix can be efﬁciently found from the output of choldc:
--for (i=1;i<=n;i++) {
--  a[i][i]=1.0/p[i];
--  for (j=i+1;j<=n;j++) {
--    sum=0.0;
--    for (k=i;k<j;k++) sum -= a[j][k]*a[k][i];
--    a[j][i]=sum/p[j];
--  }
--}

