module Sets where

import Data.List (sort, minimumBy)
import Data.Map (toList, fromListWith)

type Pitch      = Int
type PitchClass = Int
type Interval   = Int

-- Intervals

opi :: Pitch -> Pitch -> Interval
opi x y = y - x

upi :: Pitch -> Pitch -> Interval
upi x y = abs $ opi x y

opci :: PitchClass -> PitchClass -> Interval
opci x y = opi x y `mod` 12 

upci :: PitchClass -> PitchClass -> Interval
upci x y = min (opci x y) (opci y x)

reduce :: (a -> a -> Interval) -> [a] -> [Interval]
reduce f (x:y:xs)   = f x y : reduce f (y:xs)
reduce f _          = []

-- Pitch Class Operations

complement :: PitchClass -> PitchClass
complement = (`mod` 12) . negate

transpose :: PitchClass -> Interval -> PitchClass
transpose p n = (p + n) `mod` 12

normalize :: PitchClass -> PitchClass
normalize p = if p <= 6 then p else complement p

-- Pitch Class Set Operations

inverse :: [PitchClass] -> [PitchClass]
inverse = map complement

transposeSet :: [PitchClass] -> Interval -> [PitchClass]
transposeSet s n = map (`transpose` n) s

transposeInverse :: [PitchClass] -> Interval -> [PitchClass]
transposeInverse s n = transposeSet (inverse s) n

zero :: [PitchClass] -> [PitchClass]
zero [] = []
zero s@(a:_) = transposeSet s (opci a 0)

normalForm :: [PitchClass] -> [PitchClass]
normalForm = minimumBy compareSets . cycles . sort

primeForm :: [PitchClass] -> [PitchClass]
primeForm s = minimumBy compareSets [s', s'']
  where
    normalize'  = zero . normalForm
    s'          = normalize' s
    s''         = normalize' $ inverse s'

-- Pitch Class Set Analysis and Comparison

setSpan :: [PitchClass] -> Interval
setSpan s = opci (head s) (head $ reverse s)

packedness :: [PitchClass] -> [Interval]
packedness = reduce opci

compareSets :: [PitchClass] -> [PitchClass] -> Ordering
compareSets a b
    | s1 /= s2  = compare s1 s2
    | otherwise = compare (packedness a) (packedness b)
  where
    s1 = setSpan a
    s2 = setSpan b

-- Pitch Class Set Intervalic Content

intervalTriangle :: [Int] -> [[Int]]
intervalTriangle []         = []
intervalTriangle [_]        = []
intervalTriangle [x,y]      = [[y - x]]
intervalTriangle (x:xs)     = row : intervalTriangle row
  where
    row = [i - x | i <- xs]

intervalFrequency :: [Interval] -> [(Interval,Int)]
intervalFrequency = frequency . sort . map normalize

intervalClassVector :: [PitchClass] -> [Interval]
intervalClassVector = frequencyVector 1 6 . intervalFrequency . concat . intervalTriangle . sort

-- Generic Functions

cycles :: [a] -> [[a]]
cycles s = [drop i s ++ take i s | i <- [0..length s - 1]]

frequency :: Ord a => [a] -> [(a,Int)]
frequency xs = toList $ fromListWith (+) [(x, 1) | x <- xs]

frequencyVector :: (Eq a, Num a) => a -> a -> [(a, Int)] -> [Int]
frequencyVector _ 0 _   = []
frequencyVector _ b []  = 0 : frequencyVector 0 (b - 1) []
frequencyVector a b xs@((x,y):ys)
    | x == a    = y : frequencyVector (a + 1) (b - 1) ys
    | otherwise = 0 : frequencyVector (a + 1) (b - 1) xs

