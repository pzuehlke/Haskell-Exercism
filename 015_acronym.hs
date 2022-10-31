import Data.Char

abbreviate :: String -> String
abbreviate = concat . acronym . split . removeDoubleSpaces

whiteSpace :: [Char]
whiteSpace = [' ', '\t', '\n', '-']

removeDoubleSpaces :: String -> String
removeDoubleSpaces cs = removeDoubleSpaces' cs 'n'

removeDoubleSpaces' :: String -> Char -> String
removeDoubleSpaces' [] _         = []
removeDoubleSpaces' (c:cs) 'w'
    | c `elem` whiteSpace   = removeDoubleSpaces' cs 'w'
    | otherwise             = c : (removeDoubleSpaces' cs 'n')
removeDoubleSpaces' (c:cs) 'n'
    | c `elem` whiteSpace   = ' ' : (removeDoubleSpaces' cs 'w')
    | otherwise             = c : (removeDoubleSpaces' cs 'n')

split :: String -> [String]
split cs = split' cs []

split' :: String -> [String] -> [String]
split' [] css    = css
split' (c:cs) css
    | isWhiteSpace c        = split' (dropWhile (not . isWhiteSpace) cs)
                                    (css ++ [takeWhile (not . isWhiteSpace) cs])
    | otherwise             = split' (dropWhile (not . isWhiteSpace) (c:cs))
                                    (css ++ [takeWhile (not . isWhiteSpace) (c:cs)])

isWhiteSpace :: Char -> Bool
isWhiteSpace = (`elem` whiteSpace)

getFirstCases :: String -> String
getFirstCases []        = []
getFirstCases (c:cs)
    | all isUpper (c:cs)    = [toUpper c]
    | otherwise             = (toUpper c : cs)

acronym :: [String] -> [String]
acronym = map (filter isUpper) . map (filter isAlpha) . map getFirstCases
