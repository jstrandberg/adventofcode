example :: String
example = "A Y\nB X\nC Z"

score :: String -> Int
score "A X" = 3 + 1
score "A Y" = 6 + 2
score "A Z" = 0 + 3
score "B X" = 0 + 1
score "B Y" = 3 + 2
score "B Z" = 6 + 3
score "C X" = 6 + 1
score "C Y" = 0 + 2
score "C Z" = 3 + 3

score' :: String -> Int
score' "A X" = 0 + 3
score' "A Y" = 3 + 1
score' "A Z" = 6 + 2
score' "B X" = 0 + 1
score' "B Y" = 3 + 2
score' "B Z" = 6 + 3
score' "C X" = 0 + 2
score' "C Y" = 3 + 3
score' "C Z" = 6 + 1

main :: IO ()
main = do
  putStrLn "What to do..."
  putStrLn $ show $ sum $ map score $ lines example
  putStrLn $ show $ sum $ map score' $ lines example
  {-
  file <- readFile "input.txt"
  putStrLn $ show $ sum $ map score $ lines file
  putStrLn $ show $ sum $ map score' $ lines file
  -}
