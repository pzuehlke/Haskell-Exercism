import Data.Map (Map, fromList)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

dnaNucleotides :: String
dnaNucleotides = "ACGT"

invalidDNA :: String -> Bool
invalidDNA = not . (all (`elem` dnaNucleotides))

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
    | invalidDNA xs = Left "Invalid DNA sequence!"
    | otherwise     = Right (formatCount xs)

convertToChar :: Nucleotide -> Char
convertToChar A = 'A'
convertToChar C = 'C'
convertToChar G = 'G'
convertToChar T = 'T'

convertFromChar :: Char -> Nucleotide
convertFromChar 'A' = A
convertFromChar 'C' = C
convertFromChar 'G' = G
convertFromChar 'T' = T

countLetter :: Eq a => a -> [a] -> Int
countLetter x = length . filter (== x)

formatCount :: String -> Map Nucleotide Int
formatCount xs = fromList [(convertFromChar c, countLetter c xs) |
                           c <- dnaNucleotides]
