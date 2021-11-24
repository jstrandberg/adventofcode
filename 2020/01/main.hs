example :: String
example = "1721\n979\n366\n299\n675\n1456\n"

entries :: String -> [Integer]
entries s = map read $ lines s

combine :: [Integer] -> [[Integer]]
combine [] = []
combine (x:xs) = map (\num -> [x,num]) xs ++ combine xs

find2020 :: [[Integer]] -> [Integer]
find2020 list = head $ dropWhile (\x -> sum x /= 2020) list

result1 :: String -> String
result1 s = show $ product $ find2020 $ combine $ entries s

result2 :: String -> String
result2 s = "I don't know..."

main :: IO ()
main = do
    putStrLn "What to do..."
    input <- readFile "input.txt"
    putStrLn $ result1 example
    putStrLn $ result1 input