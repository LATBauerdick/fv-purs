module Main where
import Prelude

import Effect ( Effect )

import Test.Main (main) as T

main :: Effect Unit
main = do
  T.main
