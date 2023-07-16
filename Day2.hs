module Day2 where

import Data.Char
import Lib

-- Magic.

parser :: String -> [(Int, Int)]
parser = pairs . map ord . concat . words

partA :: String -> Int
partA = sum . map score . parser
  where score (x, y) = y - 87 + (y - x - 1) `mod` 3 * 3
-- Unminimized
--where
--  score x y = (y - (ord 'W') + outcomeScore x y)
--  outcomeScore x y = (y - x + (ord 'A' - ord 'X' + 1)) `mod` 3 * 3

partB :: String -> Int
partB = sum . map score . parser
  where score (x, y) = (x + y - 1) `mod` 3 + y * 3 - 263
-- Unminimized
--where 
--  score x y = rightMove x y + outcomeScore y
--  rightMove x y = 1 + (x + y - ord 'A' - ord 'X' + 2) `mod` 3
--  outcomeScore y = 3 * (y - ord 'X')

main :: IO ()
main = allParts 2 [partA, partB]
