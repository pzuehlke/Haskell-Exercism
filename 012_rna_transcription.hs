dnaNucleotides :: String
dnaNucleotides = "ACGT"

convert :: Char -> Char
convert 'A' = 'U'
convert 'C' = 'G'
convert 'G' = 'C'
convert 'T' = 'A'

invalidDNA :: String -> Bool
invalidDNA = not . (all (`elem` dnaNucleotides))

firstInvalid :: String -> Char
firstInvalid = head . (filter (not . (`elem` dnaNucleotides)))

toRNA' :: String -> String
toRNA' []        = "" 
toRNA' (c:cs)    = (convert c) : (toRNA' cs)

toRNA :: String -> Either Char String
toRNA []        = Right ""
toRNA (c:cs)
    | invalidDNA (c:cs)     = Left (firstInvalid (c:cs))
    | otherwise             = Right (convert c : toRNA' cs)
