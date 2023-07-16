module Lib where

-- Running solutions
allParts :: Show a => Int -> [String -> a] -> IO ()
allParts day parts = unlines . map show . multipart parts
  <$> (readFile ("inputs/day" ++ show day ++ ".txt"))
  >>= putStrLn

multipart :: [a -> b] -> a -> [b]
multipart solns input = map ($ input) solns

parseBlock :: Read a => String -> [[a]]
parseBlock = map (map read) . parse' . lines
  where 
    parse' str = case break null str of
      (a, "":b) -> a : parse' b
      (a, []) -> [a]

-- List business
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- Tuple business
parsePair :: Char -> String -> (String, String)
parsePair sep s = (a, tail b)
  where (a, b) = break (== sep) s

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a:b:xs) = (a, b) : pairs xs
pairs _ = error "odd number of elements"

triplets :: [a] -> [(a, a, a)]
triplets [] = []
triplets (a:b:c:xs) = (a, b, c) : triplets xs
triplets _ = error "number of elements cannot be evenly divided by three"

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

intersect3 :: Eq a => [a] -> [a] -> [a] -> [a]
intersect3 xs ys zs = [x | x <- xs, any (== x) ys, any (== x) zs]

-- Ranges
type Range = [Int]

parseRange :: String -> Range
parseRange s = [a..b]
  where (a, b) = both read $ sepPair '-' s
  
fullyContains :: Range -> Range -> Bool
fullyContains a b = union == a || union == b
  where union = intersect a b

overlaps :: Range -> Range -> Bool 
overlaps a b = not $ null $ intersect a b
