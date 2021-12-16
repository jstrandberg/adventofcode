binaryFrom :: Char -> String
binaryFrom c
  | c == '0' = "0000"
  | c == '1' = "0001"
  | c == '2' = "0010"
  | c == '3' = "0011"
  | c == '4' = "0100"
  | c == '5' = "0101"
  | c == '6' = "0110"
  | c == '7' = "0111"
  | c == '8' = "1000"
  | c == '9' = "1001"
  | c == 'A' = "1010"
  | c == 'B' = "1011"
  | c == 'C' = "1100"
  | c == 'D' = "1101"
  | c == 'E' = "1110"
  | c == 'F' = "1111"

-- stolen from iceman@stackoverflow
intFrom :: String -> Int
intFrom = foldl (\a n -> a * 2 + (read [n])) 0

example = concat . map binaryFrom $ "D2FE28"

parseLiteral bs = typeid





load bits = load' bits'
  where
    bits' = drop 6 bits
    load'

version = intFrom . take 3
typeid  = intFrom . take 3 . drop 3
