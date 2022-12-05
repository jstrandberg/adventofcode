import Data.List
import Data.Maybe

example :: String
example = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"

parse :: String -> [(String,String)]
parse = map split . lines
  where
    split s = splitAt (length s `div` 2) s

parse' :: String -> [[String]]
parse' s = parse'' [] $ lines s
  where
    parse'' rs [] = rs
    parse'' rs (s1:s2:s3:ss) = parse'' ([s1,s2,s3] : rs) ss

prio :: Char -> Int
prio c = fromJust r
  where
    l = zip ['a'..'z'] [1..] ++ zip ['A'..'Z'] [27..]
    r = snd <$> find ((== c) . fst) l

common :: (String,String) -> Char
common = fn Nothing
  where
    fn Nothing ((a:as),b) = fn (find (== a) b) (as,b)
    fn (Just c) _ = c

common' :: [String] -> Char
common' (s:ss) = fromJust $ fmap snd $ find fst $ zip smap s
  where
    find' c = map (find (== c)) ss
    smap = map (all isJust) $ map find' s

main :: IO ()
main = do
  putStrLn "What to do..."
  putStrLn $ show $ sum $ map (prio . common) $ parse example
  putStrLn $ show $ sum $ map (prio . common') $ parse' example
  {-
  file <- readFile "input.txt"
  putStrLn $ show $ sum $ map (prio . common) $ parse file
  putStrLn $ show $ sum $ map (prio . common') $ parse' file
  -}
