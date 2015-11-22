module Main where

import System.IO()
import Data.Char

-- Problem statement: https://community.topcoder.com/stat?c=problem_statement&pm=4788
-- Method signature: int newFunction(String code)

newFunction :: String -> Int
newFunction input =
    let arr = [6, 2, 5, 5, 4, 5, 6, 3, 7, 6]
        x = [arr !! digitToInt (input !! x) | x <- [0..length input - 1]]
        in sum x

assert :: String -> Int -> String
assert s i = if newFunction s == i
then "test passed" else "test failed"


main :: IO()
main = do
  putStrLn $ show $ assert "13579" 21
  putStrLn $ show $ assert "868177" 28
  putStrLn $ show $ assert "8571" 17
  putStrLn $ show $ assert "3" 5
  putStrLn $ show $ assert "73254370932875002027963295052175" 157
