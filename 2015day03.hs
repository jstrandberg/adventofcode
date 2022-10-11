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

calc :: String -> Int
calc = length . group . sortBy order . scanl move (0,0)

calc' :: String -> Int
calc' s = let
  santa = scanl move (0,0) $ map snd $ filter (even . fst) $ zip [0..] s
  robot = scanl move (0,0) $ map snd $ filter (odd  . fst) $ zip [0..] s
  in length $ group $ sortBy order $ concat [santa,robot]
main :: IO ()
main = do
  putStrLn "What to do..."
  putStrLn $ show $ calc example
  putStrLn $ show $ calc' example
  {-
  file <- readFile "input.txt"
  putStrLn $ show $ calc file
  putStrLn $ show $ calc' file
  -}
