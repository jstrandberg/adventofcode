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

-- middle char, preference high
middle :: String -> Char
middle s = s !! (div (length s) 2)

flipBit :: Char -> Char
flipBit c = if c == '0' then '1' else '0'

-- least common bit, preference 0
lcBit :: String -> Char
lcBit s = flipBit $ middle $ sort s

-- most common bit, preference 1
mcBit :: String -> Char
mcBit s = middle $ sort s

gamma :: [String] -> String
gamma xs = map mcBit $ transpose xs

epsilon :: [String] -> String
epsilon xs = map lcBit $ transpose xs

-- part 2
-- collecting result in accumulator
-- recursivly filtering and getting tails
-- bailing on single hit

oxygen :: [String] -> String -> String
oxygen (s : []) acc = acc ++ s
oxygen xs acc =
    let bit = mcBit $ head $ transpose xs
        xs' = filter (\s -> head s == bit) xs
    in  oxygen (map tail xs') $ acc ++ [bit]

carbon :: [String] -> String -> String
carbon (s : []) acc = acc ++ s
carbon xs acc =
    let bit = lcBit $ head $ transpose xs
        xs' = filter (\s -> head s == bit) xs
    in  carbon (map tail xs') $ acc ++ [bit]

-- stolen from iceman@stackoverflow
binaryToInt :: String -> Int
binaryToInt s = foldl (\a n -> a * 2 + (read [n])) 0 s

result :: [String] -> Int
result s = (binaryToInt $ gamma s) * (binaryToInt $ epsilon s)

result' :: [String] -> Int
result' s = (binaryToInt $ oxygen s "") * (binaryToInt $ carbon s "")

main :: IO ()
main = do
    putStrLn "What to do..."
    putStrLn $ show $ result example
    putStrLn $ show $ result' example
    {-
    file <- readFile "input.txt"
    putStrLn $ show $ result $ lines file
    putStrLn $ show $ result' $ lines file
    -}
