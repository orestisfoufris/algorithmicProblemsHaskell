module Main where

-- http://codeforces.com/problemset/problem/592/B
main :: IO()
main = do
    input <- readLn :: IO Integer
    putStrLn $ show $ (input - 2) ^ 2
