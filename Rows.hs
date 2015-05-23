module Rows where

import Sets

type Row    = [PitchClass]
type Matrix = [Row]

rowMatrix :: Row -> Matrix
rowMatrix r = [transposeSet i r' | i <- inverse r']
  where
    r' = zero r

-- Prime
pRow :: Int -> Row -> Row
pRow n = transposeSet n . zero

-- Retrograde
rRow :: Int -> Row -> Row
rRow n = reverse . pRow n

-- Inverse
iRow :: Int -> Row -> Row
iRow n = pRow n . inverse

-- Retrograde Inverse
riRow :: Int -> Row -> Row
riRow n = reverse . iRow n

