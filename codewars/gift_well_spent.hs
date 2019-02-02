-- https://www.codewars.com/kata/a-gift-well-spent/train/haskell

import qualified Data.Map as M
import Data.List
import Data.Maybe

buy :: (Ord a, Num a, Eq a) => a -> [a] -> Maybe (Int, Int)
buy c is
    | size == 0 = Nothing
    | size == 1 && isNothing (head list) = Nothing
    | otherwise = head $ filter isJust list
    where list = sort $ solve c (zip is [0..]) M.empty []
          size = length list

solve :: (Ord k, Num k) => k -> [(k, a)] -> M.Map k a -> [Maybe (a, a)] -> [Maybe (a, a)]
solve _ [] _ l = l ++ [Nothing]
solve t ((f, s):ts) m l = case M.lookup (t - f) m of
    Just x -> Just (x, s) : solve t ts m l
    Nothing -> solve t ts (checkKey f s m) l

checkKey :: Ord k => k -> a -> M.Map k a -> M.Map k a
checkKey key val map = case M.lookup key map of
  Just x -> map
  Nothing -> M.insert key val map
