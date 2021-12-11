example :: [String]
example = ["be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
          ,"edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
          ,"fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
          ,"fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
          ,"aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
          ,"fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
          ,"dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
          ,"bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
          ,"egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
          ,"gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
          ]

output :: String -> [String]
output s = words . last . lines $ [if c == '|' then '\n' else c | c <- s]

input :: String -> [String]
input s = words . head . lines $ [if c == '|' then '\n' else c | c <- s]

isUnique :: String -> Bool
isUnique s
    | length s == 2 = True
    | length s == 3 = True
    | length s == 4 = True
    | length s == 7 = True
    | otherwise     = False

-- count how many segments of a is not present in b
diff :: String -> String -> Int
diff a b = foldl (\d c -> if elem c b then d else d + 1) 0 a

-- are a and b equal segement-wise?
equal :: String -> String -> Bool
equal a b
    | length a /= length b = False
    | diff a b /= 0        = False
    | otherwise            = True

-- create tuple map of input segments
numberMap :: [String] -> [(Int,String)]
numberMap x = zip [0..9] $ map (flip id x)
        [zero,one,two,three,four,five,six,seven,eight,nine]

-- get output number from input and output segments
number :: [String] -> [String] -> Int
number i o = concatNum . map findNumber $ o
    where findNumber s = fst . head . filter (equal s . snd) $ numberMap i
          concatNum x  = read . concat . map show $ x

one :: [String] -> String
one x = head $ filter ((==) 2 . length) x

two :: [String] -> String
two x = head $ filter diff4is2 length5
    where length5    = filter ((==) 5 . length) x
          diff4is2 s = diff (four x) s == 2

three :: [String] -> String
three x = head $ filter contain7 length5
    where length5    = filter ((==) 5 . length) x
          contain7 s = diff (seven x) s == 0

four :: [String] -> String
four x = head $ filter ((==) 4 . length) x

five :: [String] -> String
five x = head $ filter isNot2or3 length5
    where length5     = filter ((==) 5 . length) x
          isNot2or3 s = s /= two x && s /= three x

six :: [String] -> String
six x = head $ filter diff1is1 length6
    where length6    = filter ((==) 6 . length) x
          diff1is1 s = diff (one x) s == 1

seven :: [String] -> String
seven x = head $ filter ((==) 3 . length) x

eight :: [String] -> String
eight x = head $ filter ((==) 7 . length) x

nine :: [String] -> String
nine x = head $ filter contain4 length6
    where length6    = filter ((==) 6 . length) x
          contain4 s = diff (four x) s == 0

zero :: [String] -> String
zero x = head $ filter isNot6or9 length6
    where length6     = filter ((==) 6 . length) x
          isNot6or9 s = s /= six x && s /= nine x

result :: [String] -> Int
result x = length . filter isUnique $ outputs
    where outputs = x >>= output

result' :: [String] -> Int
result' x = sum . map number' $ x
    where number' s = number (input s) (output s)

main :: IO ()
main = do
    putStrLn "What to do..."
    putStrLn . show . result $ example
    putStrLn . show . result' $ example
    {-
    file <- readFile "input.txt"
    putStrLn . show . result . lines $ file
    putStrLn . show . result' . lines $ file
    -}
