example :: [Int]
example = [199,200,208,210,200,207,240,269,260,263]

calc :: Int -> [Int] -> Int
calc count []  = count
calc count [_] = count
calc count (a:b:xs) =
    if b > a then
        calc (count + 1) (b:xs)
    else
        calc count (b:xs)

main :: IO ()
main = do
    putStrLn "What to do..."
    putStrLn $ show $ calc 0 example
