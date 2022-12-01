import Data.List
import Data.Ord

example :: String
example = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"

parse :: String -> [[Int]]
parse = foldr fn [[]] . lines
  where
    fn a (b:bs) =
      if a == ""
        then [] : b : bs
        else (read a : b) : bs

calc :: [[Int]] -> Int
calc = maximum . map sum

-- replaced "reverse . sort" with a more efficient alternative
-- https://ro-che.info/articles/2016-04-02-descending-sort-haskell
calc' :: [[Int]] -> Int
calc' = sum . take 3 . sortBy (comparing Down) . map sum

main :: IO ()
main = do
  putStrLn "What to do..."
  putStrLn $ show $ calc $ parse example
  putStrLn $ show $ calc' $ parse example
  {-
  file <- readFile "input.txt"
  putStrLn $ show $ calc $ parse file
  putStrLn $ show $ calc' $ parse file
  -}
