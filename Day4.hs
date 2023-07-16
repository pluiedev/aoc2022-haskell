module Day4 where

import Data.List
import Lib

parser :: String -> [(Range, Range)]
parser = map (both range . sepPair ',') . lines

partA :: String -> Int
partA = length . filter (uncurry fullyContains) . parser

partB :: String -> Int
partB = length . filter (uncurry overlaps) . parser

main :: IO ()
main = allParts 4 [partA, partB]
