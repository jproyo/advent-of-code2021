module Day1 where

import           Relude

parseInput :: Text -> [Int]
parseInput = fromMaybe [] . traverse (readMaybe . toString) . lines 

problem1 :: FilePath -> IO Int
problem1 = day1 1

problem2 :: FilePath -> IO Int
problem2 = day1 3

day1 :: Int -> FilePath -> IO Int
day1 byN = fmap (sum . uncurry (zipWith (\a b -> if a < b then 1 else 0)) . (id &&& drop byN) . parseInput) . readFileText
