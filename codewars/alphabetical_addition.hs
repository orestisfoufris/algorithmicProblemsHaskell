module AddingUpLetters (addLetters) where

-- https://www.codewars.com/kata/alphabetical-addition/train/haskell

import qualified Data.Map as M
import Data.Maybe

mapf :: M.Map Char Int
mapf = 
  let l = [x | x <- ['a'..'z']]
      n = [x | x <- [1..26]]
  in M.fromList $ zip l n
  
mapr :: M.Map Int Char
mapr = 
  let l = [x | x <- ['a'..'z']]
      n = [x | x <- [1..26]]
  in M.fromList $ zip n l

addLetters :: [Char] -> Char
addLetters [] = 'z'
addLetters x =
  let s = fromMaybe 0 (sumLetters x mapf)
  in fromMaybe 'z' (M.lookup (s `mod` 26) mapr)

sumLetters :: [Char] -> M.Map Char Int -> Maybe Int
sumLetters list map = 
  let l = fmap (\x -> M.lookup x map) list
  in foldr (\x y -> (+) <$> x <*> y) (Just 0) l
