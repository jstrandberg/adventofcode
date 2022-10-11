import Data.List

example :: String
example = "^>v<"

move :: (Int,Int) -> Char -> (Int,Int)
move (x,y) c
  | c == '^' = (x,y-1)
  | c == 'v' = (x,y+1)
  | c == '<' = (x-1,y)
  | c == '>' = (x+1,y)
  | otherwise = (x,y)

order :: (Int,Int) -> (Int,Int) -> Ordering
order (x1,y1) (x2,y2)
  | x1 < x2 = LT
  | x1 > x2 = GT
  | y1 < y2 = LT
  | y1 > y2 = GT
  | otherwise = EQ

main :: IO ()
main = do
  putStrLn "What to do..."
  putStrLn $ show $ length $ group $ sortBy order $ scanl move (0,0) example
  {-
  file <- readFile "input.txt"
  putStrLn $ show $ length $ group $ sortBy order $ scanl move (0,0) file
  -}
