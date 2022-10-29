import Data.Char

isPangram :: String -> Bool
isPangram text = isSubset alphabet (filterLetters text)

filterLetters :: String -> String
filterLetters text = [toLower c | c <- text,
                                  ord 'a' <= ord (toLower c) &&
                                  ord (toLower c) <= ord 'z']

alphabet :: [Char]
alphabet = [chr n | n <- [ord 'a'..ord 'z']]

isSubset :: String -> String -> Bool
isSubset [] ds          = True
isSubset (c:cs) ds      = (c `elem` ds) && isSubset cs ds
