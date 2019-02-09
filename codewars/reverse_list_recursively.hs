-- https://www.codewars.com/kata/reverse-list-recursively/haskell

revR :: [Int] -> [Int]
revR a
    | null a = []
    | length a == 1 = [head a]
    | otherwise = revR (tail a) ++ [head a]