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

-- examples
example1 = concat . map binaryFrom $ "D2FE28"
example2 = concat . map binaryFrom $ "38006F45291200"

data Packet =
  Literal { version :: Int } |
  Operator { l :: Length
           , packets :: [Packet]
           }
  deriving Show

data Length = Bits Int | Packets Int
  deriving Show

readPkt (Packets 0) _ = []
readPkt (Bits 0) _ = []
readPkt (Packets n) bs = case readTypeId bs of
  4 ->
    Literal { version = readLiteralLength bs } : readPkt (Packets (n-1)) (drop (readLiteralLength bs) bs)
  otherwise ->
    Operator { l = readSubLength bs
             , packets = readPkt (readSubLength bs) $ dropHeader bs } : []
readPkt (Bits n) bs = case readTypeId bs of
 4 ->
   Literal { version = readLiteralLength bs } : readPkt (Bits (n-(readLiteralLength bs))) (drop (readLiteralLength bs) bs)
 otherwise ->
   Operator { l = readSubLength bs
            , packets = readPkt (readSubLength bs) $ dropHeader bs } : []



-- literal packages
readLiteralLength = (+) 6 . (*) 5 . length . readLiteralChunks . dropHeader
readLiteralInt = intFrom . concat . readLiteralChunks . dropHeader
readLiteralChunks bs =
  let (x:a:b:c:d:xs) = bs
  in if x == '1'
    then [a,b,c,d] : readLiteralChunks xs
    else [a,b,c,d] : []

-- headers
readVersion = intFrom . take 3
readTypeId = intFrom . take 3 . drop 3
readLengthTypeId bs = read [head $ drop 6 bs]
readSubLength bs = case readLengthTypeId bs of
  0 -> Bits (intFrom . take 15 . tail . drop 6 $ bs)
  1 -> Packets (intFrom . take 11 . tail . drop 6 $ bs)
readHeaderLength bs =
  if readTypeId bs == 4
    then 6
    else if readLengthTypeId bs == 0
      then 6 + 1 + 15
      else 6 + 1 + 11
dropHeader bs = drop (readHeaderLength bs) bs
