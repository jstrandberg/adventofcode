example :: String
example = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc\n"

get_min :: String -> Int
get_min s = read
    $ takeWhile (\c -> c /= '-') s

get_max :: String -> Int
get_max s = read
    $ takeWhile (\c -> c /= ' ')
    $ tail
    $ dropWhile (\c -> c /= '-') s

get_char :: String -> Char
get_char s = head
    $ tail
    $ dropWhile (\c -> c /= ' ') s

get_string :: String -> String
get_string s = (tail . tail)
    $ dropWhile (\c -> c /= ':') s

count_char :: Char -> String -> Int
count_char c s = length $ filter (== c) s

is_valid :: Int -> Int -> Int -> Bool
is_valid _min _max count = _min <= count && count <= _max

result1 :: String -> String
result1 s = "Don't know..."

main :: IO ()
main = do
    putStrLn "What to do..."
    putStrLn $ result1 example