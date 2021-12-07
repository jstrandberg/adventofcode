example :: [Int]
example = [3,4,3,1,2]

age :: Int -> [Int] -> [Int]
age x a = let
    x' = x - 1
    in if x' < 0
        then 6 : 8 : a
        else x' : a

play :: [Int] -> Int -> [Int]
play xs 0 = xs
play xs i = let
    xs' = foldr age [] xs
    in play xs' (i - 1)

result :: [Int] -> Int
result xs = length $ play xs 80

parse :: String -> [Int]
parse s = map read $ words [if c == ',' then ' ' else c | c <- s]

main :: IO ()
main = do
    putStrLn "What to do..."
    putStrLn $ show $ result example
    {-
    file <- readFile "input.txt"
    putStrLn $ show $ result $ parse file
    -}
