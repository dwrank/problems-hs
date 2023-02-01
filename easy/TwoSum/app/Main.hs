module Main (main) where

import qualified Data.Map as Map

main :: IO ()
main = do
    print $ twoSum [2,7,11,15] 9
    print $ twoSum [2,7,11,15] 22
    print $ twoSum [2,7,11,15] 26
    print $ twoSum [2,7,11,15] 14
    print $ twoSum [2,7,11,15] 99

twoSum :: [Int] -> Int -> Maybe (Int, Int)
twoSum nums target =
    let numsWithIndices = zip nums [0..]
    in twoSum' numsWithIndices Map.empty target

twoSum' :: [(Int, Int)] -> Map.Map Int Int -> Int -> Maybe (Int, Int)
twoSum' [] _ _ = Nothing
twoSum' ((num, i):rest) seen target =
    let diff = target - num
    in case Map.lookup diff seen of
         Nothing -> twoSum' rest (Map.insert num i seen) target
         Just j -> Just (j, i)
