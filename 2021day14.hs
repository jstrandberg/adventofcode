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

template :: [String] -> String
template x = head x

rules :: [String] -> [(String,Char)]
rules (_:_:xs) = map rule xs
    where rule (a:b:_:_:_:_:c:[]) = ([a,b],c)

apply :: [(String,Char)] -> String -> String
apply _ []       = []
apply _ [a]      = [a]
apply r (a:b:xs) = [a] ++ (maybeToList $ lookup [a,b] r) ++ apply r (b:xs)

result :: [String] -> Int
result x = mc - lc
    where iterations = iterate (apply $ rules x) (template x)
          occurences = map length . group . sort $ iterations !! 10
          mc = last . sort $ occurences
          lc = head . sort $ occurences

main :: IO ()
main = do
    putStrLn "What to do..."
    putStrLn . show . result $ example
    {-
    file <- readFile "input.txt"
    putStrLn . show . result . lines $ file
    -}
