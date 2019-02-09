-- https://www.codewars.com/kata/multiply-the-number/haskell

multiply :: Integer -> Integer
multiply n = 
 let k = (5 ^) . length . show $ abs n
 in k * n