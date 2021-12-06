import Data.List

example :: [String]
example = ["00100"
          ,"11110"
          ,"10110"
          ,"10111"
          ,"10101"
          ,"01111"
          ,"00111"
          ,"11100"
          ,"10000"
          ,"11001"
          ,"00010"
          ,"01010"
          ]

middle :: String -> Int
middle s = div (length s) 2

-- already forgot how this works...
gamma :: [String] -> String
gamma x = map (flip (!!) (middle $ head $ transpose x) . sort) $ transpose x

epsilon :: [String] -> String
epsilon x = map (\x -> if x == '0' then '1' else '0') $ gamma x

-- stolen from iceman@stackoverflow
binaryToInt :: String -> Int
binaryToInt s = foldl (\a n -> a * 2 + (read [n])) 0 s

result :: [String] -> Int
result s = (binaryToInt $ gamma s) * (binaryToInt $ epsilon s)

main :: IO ()
main = do
    putStrLn "What to do..."
    putStrLn $ show $ result example
    {-
    file <- readFile "input.txt"
    putStrLn $ show $ result $ lines file
    -}
