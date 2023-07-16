module Day3 where

import Data.Char
import Data.List
import Lib

partA :: String -> Int
partA = sum . map (score . head . uncurry intersect . halve) . lines

partB :: String -> Int
partB = sum . map (score . head . uncurry3 intersect3) . triplets . lines

score :: Char -> Int
score c
  | c >= 'A' && c <= 'Z' = ord c - ord 'A' + 27
  | c >= 'a' && c <= 'z' = ord c - ord 'a' + 1
  | otherwise            = error "char not in ranges A..Z or a..z"

main = allParts 3 [partA, partB]
