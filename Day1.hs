module Day1 where

import Data.List
import Lib

partA :: String -> Int
partA = foldr max 0 . map sum . parseBlock

partB :: String -> Int
partB = sum . take 3 . reverse . sort . map sum . parseBlock

main :: IO ()
main = allParts 1 [partA, partB] 

