-- https://www.codewars.com/kata/playing-with-digits

digpow :: Integer -> Integer -> Integer
digpow n p = solve (raiseToPowerAndSum (reverse $ getDigits n) p) n

getDigits :: Integer -> [Integer]
getDigits n
    | n == 0 = []
    | otherwise = [(n `mod` 10)] ++ getDigits (n `div` 10)

raiseToPowerAndSum :: [Integer] -> Integer -> Integer
raiseToPowerAndSum l p = sum $ zipWith (^) l [p ..]

solve :: Integer -> Integer -> Integer
solve s i
    | res == 0 = (s `div` i)
    | otherwise = -1
    where res = (s `mod` i)
