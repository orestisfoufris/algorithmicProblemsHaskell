-- https://www.hackerrank.com/challenges/fp-update-list/problem?h_r=next-challenge&h_v=zen

f [] = []
f lst = [if x < 0 then x * (-1) else x | x <- lst]

main = do
	inputdata <- getContents
    mapM_ putStrLn $ map show $ f $ map (read :: String -> Int) $ lines inputdata
    