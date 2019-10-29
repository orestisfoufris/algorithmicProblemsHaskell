module AddingUpLetters (addLetters) where

-- https://www.codewars.com/kata/alphabetical-addition/train/haskell

import qualified Data.Map as M
import Data.Maybe

mapf :: M.Map Char Int
mapf = M.fromList $ zip ['a'..'z'] [1..26]
  
mapr :: M.Map Int Char
mapr = M.fromList $ zip [1..26] ['a'..'z']

addLetters :: [Char] -> Char
addLetters [] = 'z'
addLetters x =
  let s = fromMaybe 0 (sumLetters x mapf)
  in fromMaybe 'z' (M.lookup (s `mod` 26) mapr)

sumLetters :: [Char] -> M.Map Char Int -> Maybe Int
sumLetters list map = 
  let l = fmap (\x -> M.lookup x map) list
  in foldr (\x y -> (+) <$> x <*> y) (Just 0) l
