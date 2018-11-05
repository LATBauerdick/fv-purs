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
  , next, genRange
  , newStdGen
  , StdGen
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

import Prelude.Extended ( undefined, bind, negate, pure, ($), (*), (-) )
{-- import Data.Traversable (mapAccumL) --}
import Effect ( Effect )
import Effect.Random ( random )
import Data.Traversable ( for )
import Data.Tuple ( Tuple (..) )
import Math ( sin, cos, log, sqrt, pi )
import Data.List ( List ( Nil ), (:),  range, concat )

newStdGen :: Effect Number
newStdGen = random

class RandomGen g where
  -- |The 'next' operation returns an 'Int' that is uniformly distributed
  -- in the range returned by 'genRange' (including both end points),
  -- and a new generator.
  next     :: g -> (Tuple Number g)
  -- |The 'genRange' operation yields the range of values returned by
  -- the generator.
  --
  -- It is required that:
  --
  -- * If @(a,b) = 'genRange' g@, then @a < b@.
  --
  -- * 'genRange' always returns a pair of defined 'Int's.
  --
  -- The second condition ensures that 'genRange' cannot examine its
  -- argument, and hence the value it returns can be determined only by the
  -- instance of 'RandomGen'.  That in turn allows an implementation to make
  -- a single call to 'genRange' to establish a generator's range, without
  -- being concerned that the generator returned by (say) 'next' might have
  -- a different range to the generator passed to 'next'.
  --
  -- The default definition spans the full range of 'Int'.
  genRange :: g -> (Tuple Number Number)
instance randomGenStd :: RandomGen StdGen  where
  -- |The 'next' operation returns a Number that is uniformly distributed
   -- in the range returned by 'genRange' (including both end points),
   -- and a new generator.
  next g = (Tuple r g') where
    r = 0.51111
    g' = g
    {-- g'' = do --}
    {--   n <- random --}
    {--   pure n --}
  genRange _ = (Tuple 0.0 1.0)

data StdGen = StdGen Number
  {-- forall e. (Effect (random :: RANDOM | e) g) --}

class Random a where
  randoms  :: forall g. RandomGen g => g -> List a
instance randomNumber :: Random Number where
  randoms g = (a:b:c:d:e:f:Nil) where
    (Tuple n g') = next g
    a = 0.123
    b = 0.234
    c = 0.345
    d = 0.456
    e = 0.567
    f = undefined -- 0.678

{-- -- | Plural variant of 'random', producing an infinite list of --}
{-- -- random values instead of returning a new generator. --}
{-- randoms :: forall g a. RandomGen g => g -> List a --}
{-- randoms g = build (\cons _nil -> buildRandoms cons random g) --}

-- | Produce an infinite list-equivalent of random values.
buildRandoms :: forall g a as. RandomGen g
              => (a -> as -> as)     -- e.g. '(:)', but subject to fusion
              -> (g -> (Tuple a g))  -- e.g. 'random'
              -> g                   -- a 'RandomGen' instance
              -> as
buildRandoms cons rand = go
  where
    -- The seq fixes part of #4218 and also makes fused Core simpler.
    go g = (x `cons` go g') where (Tuple x g') = rand g

build :: forall a. ((a -> List a -> List a) -> List a -> List a) -> List a
build g = g (:) Nil

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


-- API
-- ===
-- | Takes a random number generator g, and returns a random value
-- normally distributed with mean 0 and standard deviation 1,
-- together with a new generator. This function is analogous to
-- 'Random.random'.
normal :: forall g. RandomGen g => g -> (Tuple Number g)
normal g0 = (Tuple 0.5111 g0) where
  g1 = do
      u1 <- random
      u2 <- random
      let (Tuple uu _) = boxMuller u1 u2
      pure $ uu

-- | Plural variant of 'normal', producing an infinite list of
-- random values instead of returning a new generator. This function
-- is analogous to 'Random.randoms'.
{-- normals :: forall g. RandomGen g => g -> List Number --}
{-- normals = boxMullers <<< randoms --}
normals :: Int -> Effect (List Number)
normals n = rs where
  ns = range 0 (n-1)
  rs = do
    lln <- for ns \nn -> do
          u1 <- random
          u2 <- random
          let n1 = sqrt (-2.0 * log u1)
              n2 = 2.0 * pi * u2
          pure $ (n1 : n2 : Nil)
    pure $ concat lln
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
