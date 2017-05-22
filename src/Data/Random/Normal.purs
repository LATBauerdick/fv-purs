{- |
   Copyright  : Copyright (C) 2011 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn.buckwalter@gmail.com
   Stability  : Stable
   Portability: Haskell 98

This purpose of this library is to have a simple API and no
dependencies beyond Haskell 98 in order to let you produce normally
distributed random values with a minimum of fuss. This library does
/not/ attempt to be blazingly fast nor to pass stringent tests of
randomness. It attempts to be very easy to install and use while
being \"good enough\" for many applications (simulations, games, etc.).
The API builds upon and is largely analogous to that of the Haskell
98 @Random@ module (more recently @System.Random@).

Pure:

> (sample,g) = normal  myRandomGen  -- using a Random.RandomGen
> samples    = normals myRandomGen  -- infinite list
> samples2   = mkNormals 10831452   -- infinite list using a seed

In the IO monad:

> sample    <- normalIO
> samples   <- normalsIO  -- infinite list

With custom mean and standard deviation:

> (sample,g) = normal'    (mean,sigma) myRandomGen
> samples    = normals'   (mean,sigma) myRandomGen
> samples2   = mkNormals' (mean,sigma) 10831452

> sample    <- normalIO'  (mean,sigma)
> samples   <- normalsIO' (mean,sigma)

Internally the library uses the Box-Muller method to generate
normally distributed values from uniformly distributed random values.
If more than one sample is needed taking samples off an infinite
list (created by e.g. 'normals') will be roughly twice as efficient
as repeatedly generating individual samples with e.g. 'normal'.

-}


module Data.Random.Normal (
  -- * Pure interface
    {-- normal --}
  normals
  , class RandomGen
  , next
  {-- , mkNormals --}

  {-- -- ** Custom mean and standard deviation --}
  {-- , normal' --}
  {-- , normals' --}
  {-- , mkNormals' --}

  {-- -- * Using the global random number generator --}
  {-- , normalIO --}
  {-- , normalsIO --}

  {-- -- ** Custom mean and standard deviation --}
  {-- , normalIO' --}
  {-- , normalsIO' --}

  ) where

import Prelude
{-- import Data.Traversable (mapAccumL) --}
import Control.Monad.Eff ( Eff )
import Control.Monad.Eff.Random ( random, RANDOM )
import Data.Tuple
import Math ( sin, cos, log, sqrt, pi )
import Data.List
import Stuff

class RandomGen g where
  next     :: forall e. (Eff (random :: RANDOM | e) g) -> (Tuple Number (Eff (random :: RANDOM | e) g))
instance randomGenNumber :: RandomGen Number where
  next g = (Tuple r g') where
    r = 0.51111
    
      n <- random
      pure n
    g' = g
    {-- g' = g --}
    {-- r = do --}
    {--   n <- random --}
    {--   pure $ n --}

data StdGen = StdGen Number

class Random a where
  randoms  :: forall g. g -> List a
instance randomNumber :: Random Number where
  randoms  g = undefined

{-- -- Normal distribution approximation --}
{-- -- --------------------------------- --}
{-- -- | Box-Muller method for generating two normally distributed --}
{-- -- independent random values from two uniformly distributed --}
{-- -- independent random values. --}
boxMuller :: Number -> Number -> (Tuple Number Number)
boxMuller u1 u2 = (Tuple (r * cos t) (r * sin t)) where r = sqrt (-2.0 * log u1)
                                                        t = 2.0 * pi * u2

{-- -- | Convert a list of uniformly distributed random values into a --}
{-- -- list of normally distributed random values. The Box-Muller --}
{-- -- algorithms converts values two at a time, so if the input list --}
{-- -- has an uneven number of element the last one will be discarded. --}
boxMullers :: List Number -> List Number
boxMullers (u1:u2:us) = n1:n2:boxMullers us where (Tuple n1 n2) = boxMuller u1 u2
boxMullers _          = Nil


{-- -- API --}
{-- -- === --}
{-- -- | Takes a random number generator g, and returns a random value --}
{-- -- normally distributed with mean 0 and standard deviation 1, --}
{-- -- together with a new generator. This function is analogous to --}
{-- -- 'Random.random'. --}
{-- normal :: RandomGen g => Random a => Field a => g -> (Tuple a g) --}
{-- normal g0 = (Tuple (fst $ boxMuller u1 u2) g2) --}
{--   -- While The Haskell 98 report says "For fractional types, the --}
{--   -- range is normally the semi-closed interval [0,1)" we will --}
{--   -- specify the range explicitly just to be sure. --}
{--   where --}
{--      (Tuple u1 g1) = randomR (Tuple 0 1) g0 --}
{--      (Tuple u2 g2) = randomR (Tuple 0 1) g1 --}

-- | Plural variant of 'normal', producing an infinite list of
-- random values instead of returning a new generator. This function
-- is analogous to 'Random.randoms'.
normals :: forall g. RandomGen g => g -> List Number
normals = boxMullers <<< randoms

{-- -- | Creates a infinite list of normally distributed random values --}
{-- -- from the provided random generator seed. (In the implementation --}
{-- -- the seed is fed to 'Random.mkStdGen' to produce the random --}
{-- -- number generator.) --}
{-- mkNormals :: (Tuple (Random a) (Field a)) => Int -> List a --}
{-- mkNormals = normals <<< mkStdGen --}


{-- -- | A variant of 'normal' that uses the global random number --}
{-- -- generator. This function is analogous to 'Random.randomIO'. --}
{-- normalIO :: (Tuple (Random a) (Field a)) => IO a --}
{-- normalIO = do u1 <- randomRIO (Tuple 0 1) --}
{--               u2 <- randomRIO (Tuple 0 1) --}
{--               pure $ fst $ boxMuller u1 u2 --}

{-- -- | Creates a infinite list of normally distributed random values --}
{-- -- using the global random number generator. (In the implementation --}
{-- -- 'Random.newStdGen' is used.) --}
{-- normalsIO :: (Tuple (Random a) (Field a)) => IO (List a) --}
{-- normalsIO = fmap normals newStdGen --}


{-- -- With mean and standard deviation --}
{-- -- -------------------------------- --}
{-- -- | Analogous to 'normal' but uses the supplied (mean, standard --}
{-- -- deviation). --}
{-- normal' :: RandomGen g => Random a => Field a => (Tuple a a) -> g -> (Tuple a g) --}
{-- normal' (Tuple mean sigma) g = (Tuple (x * sigma + mean) g') where (Tuple x g') = normal g --}

{-- -- | Analogous to 'normals' but uses the supplied (mean, standard --}
{-- -- deviation). --}
{-- normals' :: RandomGen g => Random a => Field a => (Tuple a a) -> g -> List a --}
{-- normals' (Tuple mean sigma) g = map (\x -> x * sigma + mean) (normals g) --}

{-- -- | Analogous to 'mkNormals' but uses the supplied (mean, standard --}
{-- -- deviation). --}
{-- mkNormals' :: (Tuple (Random a) (Field a)) => (Tuple a a) -> Int -> List a --}
{-- mkNormals' ms = normals' ms . mkStdGen --}


{-- -- | Analogous to 'normalIO' but uses the supplied (mean, standard --}
{-- -- deviation). --}
{-- normalIO' ::(Tuple (Random a) (Field a)) => (Tuple a a) -> IO a --}
{-- normalIO' (Tuple mean sigma) = fmap (\x -> x * sigma + mean) normalIO --}

{-- -- | Analogous to 'normalsIO' but uses the supplied (mean, standard --}
{-- -- deviation). --}
{-- normalsIO' :: (Tuple (Random a) (Field a)) => (Tuple a a) -> IO (List a) --}
{-- normalsIO' ms = fmap (normals' ms) newStdGen --}
