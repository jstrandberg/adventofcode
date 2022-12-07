example :: String
example = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

check :: String -> Bool
check [] = False
check (x:xs) = elem x xs || check xs

calc :: Int -> String -> Int
calc n = calc' 0
  where
    calc' i s
      | check $ take n s = calc' (i + 1) (tail s)
      | otherwise = i + n

main :: IO ()
main = do
  putStrLn "What to do..."
  putStrLn $ show $ calc 4 example
  putStrLn $ show $ calc 14 example
  {-
  file <- readFile "input.txt"
  putStrLn $ show $ calc 4 file
  putStrLn $ show $ calc 14 file
  -}
