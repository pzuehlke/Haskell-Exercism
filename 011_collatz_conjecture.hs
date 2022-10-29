collatz :: Integer -> Maybe Integer
collatz n
    | n <= 0    = Nothing
    | n == 1    = Just 0
    | odd n     = Just (fromMaybe (collatz (3 * n + 1)) + 1)
    | even n    = Just (fromMaybe (collatz (n `div` 2)) + 1)


fromMaybe :: Maybe Integer -> Integer
fromMaybe (Just n) = n
fromMaybe Nothing = error "Nothing has no associated value!"
