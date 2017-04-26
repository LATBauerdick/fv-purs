module Stuff (gcd', diagonal) where

import Prelude
import Math (sqrt)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w*w + h*h)

gcd' :: Int -> Int -> Int
gcd' n 0 = n
gcd' 0 m = m
gcd' n m | n > m = gcd' (n - m) m
         | otherwise = gcd' n (m - n)
