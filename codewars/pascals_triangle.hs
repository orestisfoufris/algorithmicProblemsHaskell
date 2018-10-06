-- https://www.codewars.com/kata/5226eb40316b56c8d500030f
pascalsTriangle :: Integer -> [Integer]
pascalsTriangle n = concat [(get_row x) | x <- [0..n-1]]

get_row :: Integer -> [Integer]
get_row 0 = [1]
get_row 1 = [1, 1]
get_row n = [binom n x | x <- [0..n] ]

binom :: Integer -> Integer -> Integer
binom = loop 1 1
    where
    loop rn rd _ 0 = rn `div` rd
    loop _  _  0 _ = 0
    loop rn rd n k = loop (rn * n) (rd * k) (n-1) (k-1)
