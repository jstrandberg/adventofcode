example :: [Int]
example = [199,200,208,210,200,207,240,269,260,263]

calc :: Int -> [Int] -> Int
calc count []  = count
calc count [_] = count
calc count (a:b:xs) =
    if b > a
        then calc (count + 1) (b:xs)
        else calc count (b:xs)

slide :: [Int] -> [Int]
slide []         = []
slide [_]        = []
slide [_,_]      = []
slide (a:b:c:xs) = [a + b + c] ++ slide (b:c:xs)

parse :: String -> [Int]
parse s = map read $ lines s

main :: IO ()
main = do
    putStrLn "What to do..."
    putStrLn $ show $ calc 0 example
    putStrLn $ show $ calc 0 $ slide example
    {-
    file <- readFile "input.txt"
    putStrLn $ show $ calc 0 $ parse file
    putStrLn $ show $ calc 0 $ slide $ parse file
    -}
