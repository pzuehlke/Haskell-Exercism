import Data.Char

anagramsFor :: String -> [String] -> [String]
anagramsFor xs yss = [ys | ys <- yss,
                           areAnagrams xs' (map toLower ys)
                           && xs' /= (map toLower ys)]
    where
        xs' = map toLower xs

areAnagrams :: Eq a => [a] -> [a] -> Bool
areAnagrams [] ys           = (length ys == 0)
areAnagrams (x:xs) ys
    | (1 + occurrences x xs) == (occurrences x ys)  = areAnagrams xs' ys'
    | otherwise                                     = False     
  where
      xs' = [c | c <- xs, c /= x]
      ys' = [d | d <- ys, d /= x]

occurrences :: Eq a => a -> [a] -> Int
occurrences x []        = 0
occurrences x (y:ys)
    | x == y            = 1 + occurrences x ys
    | otherwise         = occurrences x ys

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst x []        = []
removeFirst x (y:ys)
    | x == y            = ys
    | otherwise         = y : (removeFirst x ys)
