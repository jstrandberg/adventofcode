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

calc :: [String] -> Int
calc = sum . map score

main :: IO ()
main = do
  putStrLn "What to do..."
  putStrLn $ show $ calc $ lines example
  {-
  file <- readFile "input.txt"
  putStrLn $ show $ calc $ lines file
  -}
