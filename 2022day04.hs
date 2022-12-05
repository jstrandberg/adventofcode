example :: String
example = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"

parse :: String -> [((Int,Int),(Int,Int))]
parse s = map (\[a,b,c,d] -> ((a,b),(c,d))) ss
  where
    ss = map (map read . words) $ lines $ s'
    s' = [if elem c "-," then ' ' else c | c <- s]

doubleWork :: ((Int,Int),(Int,Int)) -> Bool
doubleWork ((a,b),(c,d))
  | b - a > d - c = a <= c && b >= d
  | b - a < d - c = c <= a && d >= b
  | otherwise = a == c && b == d

doubleWork' :: ((Int,Int),(Int,Int)) -> Bool
doubleWork' ((a,b),(c,d))
  | a >= c && a <= d = True
  | b >= c && b <= d = True
  | c >= a && c <= b = True
  | otherwise = False

main :: IO ()
main = do
  putStrLn "What to do..."
  putStrLn $ show $ length $ filter id $ map doubleWork $ parse example
  putStrLn $ show $ length $ filter id $ map doubleWork' $ parse example
  {-
  file <- readFile "input.txt"
  putStrLn $ show $ length $ filter id $ map doubleWork $ parse file
  putStrLn $ show $ length $ filter id $ map doubleWork' $ parse file
  -}
