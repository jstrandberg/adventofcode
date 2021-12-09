import Data.List

example :: [Int]
example = [16,1,2,0,4,2,7,1,2,14]

from :: [Int] -> Int
from x = (head . sort) x

to :: [Int] -> Int
to x = (last . sort) x

possibilities :: [Int] -> [Int]
possibilities x = [from x .. to x]

play xs = map (\n -> sum $ distance xs n) distances
    where distance x a = map (\n -> abs $ a - n) x

result :: [Int] -> Int
result x = head $ sort $ play x

parse :: String -> [Int]
parse s = map read $ words [if c == ',' then ' ' else c | c <- s]

main :: IO ()
main = do
    putStrLn "What to do..."
    putStrLn $ show $ result example

    file <- readFile "input.txt"
    putStrLn $ show $ result $ parse file
