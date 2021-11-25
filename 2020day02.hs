example :: String
example = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc\n"

result1 :: String -> String
result1 s = "Don't know..."

main :: IO ()
main = do
    putStrLn "What to do..."
    putStrLn $ result1 example