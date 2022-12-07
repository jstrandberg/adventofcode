import Data.Char (isSpace)
import Data.List
import Control.Monad.State.Lazy

example :: String
example = "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"

pick :: Int -> State [String] Char
pick n = state (\s -> (head $ s !! (n-1), [if i == n then tail x else x | (i,x) <- zip [1..] s]))

place :: Int -> Char -> State [String] ()
place n c = state (\s -> ((), [if i == n then c : x else x | (i,x) <- zip [1..] s]))

move :: Int -> Int -> State [String] ()
move a b = do
    x <- pick a
    place b x

move' :: Int -> Int -> Int -> State [String] [()]
move' r a b = do
  let pick' = sequence $ take r $ repeat $ pick a
  x <- pick'
  sequence $ map (place b) $ reverse x

parse_init :: String -> [String]
parse_init
  = map (dropWhile isSpace . snd)
  . filter (\n -> (fst n) `mod` 4 - 1 == 0)
  . zip [0..]
  . transpose
  . takeWhile (elem '[')
  . lines

parse_cmds :: String -> [[Int]]
parse_cmds
  = map (map (read . snd))
  . map (filter (odd . fst))
  . map (zip [0..] . words)
  . tail
  . dropWhile (/= "")
  . lines

map_cmds :: [[Int]] -> [State [String] ()]
map_cmds = concat . map (\[r,a,b] -> take r $ repeat $ move a b)

result :: String -> String
result i = map head $ execState (sequence c) s
  where
    s = parse_init i
    c = map_cmds $ parse_cmds i

result' :: String -> String
result' i = map head $ execState (sequence c) s
  where
    s = parse_init i
    c = map (\[r,a,b] -> move' r a b) $ parse_cmds i

main :: IO ()
main = do
  putStrLn "What to do..."
  putStrLn $ result example
  putStrLn $ result' example
  {-
  file <- readFile "input.txt"
  putStrLn $ result file
  putStrLn $ result' file
  -}
