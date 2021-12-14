import Data.Maybe
import Data.List

example :: [String]
example =
    ["NNCB"
    ,""
    ,"CH -> B"
    ,"HH -> N"
    ,"CB -> H"
    ,"NH -> C"
    ,"HB -> C"
    ,"HC -> B"
    ,"HN -> C"
    ,"NN -> C"
    ,"BH -> H"
    ,"NC -> B"
    ,"NB -> B"
    ,"BN -> B"
    ,"BB -> N"
    ,"BC -> B"
    ,"CC -> N"
    ,"CN -> C"
    ]

type Pair = (Char,Char)
type Rule = (Pair,[Pair])

parseInput :: [String] -> [(Pair,Int)]
parseInput = pairs . head
    where pairs []       = []
          pairs [a]      = [((a,' '),1)]
          pairs (a:b:xs) = ((a,b),1) : pairs (b:xs)

compress :: Ord a => [(a,Int)] -> [(a,Int)]
compress = foldr compress' [] . sort
    where compress' a []     = [a]
          compress' a (x:xs) = if fst x == fst a
              then ((fst x, snd x + snd a) : xs)
              else (a : x : xs)

parseRules :: [String] -> [Rule]
parseRules = map rule . drop 2
    where rule (a:b:_:_:_:_:c:[]) = ((a,b),[(a,c),(c,b)])

apply :: [Rule] -> [(Pair,Int)] -> [(Pair,Int)]
apply rules = foldl apply' []
    where apply' res (pair,n) = (++) res . map (,n) . fromMaybe [pair] . lookup pair $ rules

result :: Int -> [String] -> Int
result i x = mc - lc
    where iterations = iterate (compress . apply (parseRules x)) (parseInput x)
          occurences = map snd . compress . map (\(pair,n) -> (fst pair,n)) $ iterations !! i
          mc = last . sort $ occurences
          lc = head . sort $ occurences

main :: IO ()
main = do
    putStrLn "What to do..."
    putStrLn . show . result 10 $ example
    putStrLn . show . result 40 $ example
    {-
    file <- readFile "input.txt"
    putStrLn . show . result 10 . lines $ file
    putStrLn . show . result 40 . lines $ file
    -}
