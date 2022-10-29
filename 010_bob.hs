import Data.Char

responseFor :: String -> String
responseFor xs 
  | all isSpace xs                             = reply0
  | yelling xs && last (nonWhite xs) /= '?'    = reply2
  | yelling xs && last (nonWhite xs) == '?'    = reply3
  | last (nonWhite xs) == '?'                  = reply1
  | otherwise                                  = reply4

reply0 = "Fine. Be that way!"
reply1 = "Sure."
reply2 = "Whoa, chill out!"
reply3 = "Calm down, I know what I'm doing!"
reply4 = "Whatever."

containsLower :: String -> Bool
containsLower = any isLower

yelling :: String -> Bool
yelling text = all isUpper (filterLetters text)
               && (filterLetters text) /= []

filterLetters :: String -> String
filterLetters text = [c | c <- text,
                          ord 'a' <= ord (toLower c) &&
                          ord (toLower c) <= ord 'z']

nonWhite :: String -> String
nonWhite text = [c | c <- text, not (isSpace c)]
