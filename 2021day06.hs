example :: [Int]
example = [3,4,3,1,2]

data Population = Population Int Int Int Int Int Int Int Int Int
    deriving Show

addFish :: Population -> Int -> Population
addFish (Population a b c d e f g h i) n = case n of
    0 -> Population (a + 1) b c d e f g h i
    1 -> Population a (b + 1) c d e f g h i
    2 -> Population a b (c + 1) d e f g h i
    3 -> Population a b c (d + 1) e f g h i
    4 -> Population a b c d (e + 1) f g h i
    5 -> Population a b c d e (f + 1) g h i
    6 -> Population a b c d e f (g + 1) h i
    7 -> Population a b c d e f g (h + 1) i
    8 -> Population a b c d e f g h (i + 1)
    otherwise -> error "That's a strange fish"

seed :: [Int] -> Population
seed x = foldl addFish (Population 0 0 0 0 0 0 0 0 0) x

age :: Population -> Population
age (Population a b c d e f g h i) = Population b c d e f g (a + h) i a

-- finally an infinite list
generations :: Population -> [Population]
generations p = iterate age p

result :: Population -> Int
result (Population a b c d e f g h i) = a + b + c + d + e + f + g + h + i

parse :: String -> [Int]
parse s = map read $ words [if c == ',' then ' ' else c | c <- s]

main :: IO ()
main = do
    putStrLn "What to do..."
    putStrLn $ show $ result $ generations (seed example) !! 80
    putStrLn $ show $ result $ generations (seed example) !! 256
    {-
    file <- readFile "input.txt"
    putStrLn $ show $ result $ generations (seed $ parse file) !! 80
    putStrLn $ show $ result $ generations (seed $ parse file) !! 256
    -}
