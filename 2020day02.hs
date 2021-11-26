example :: String
example = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc\n"

get_min :: String -> Int
get_min s = read
    $ takeWhile (/= '-') s

get_max :: String -> Int
get_max s = read
    $ takeWhile (/= ' ')
    $ tail
    $ dropWhile (/= '-') s

get_char :: String -> Char
get_char s = head
    $ tail
    $ dropWhile (/= ' ') s

get_string :: String -> String
get_string s = (tail . tail)
    $ dropWhile (/= ':') s

count_char :: String -> Char -> Int
count_char s c = length $ filter (== c) s

is_valid :: Int -> Int -> Int -> Bool
is_valid _min _max count = _min <= count && count <= _max

check_line :: String -> Bool
check_line s = is_valid (get_min s) (get_max s) (count_char (get_string s) $ get_char s)

result1 :: String -> String
result1 s = show 
    $ length
    $ filter (== True)
    $ map check_line
    $ lines s

main :: IO ()
main = do
    putStrLn "What to do..."
    putStrLn $ result1 example