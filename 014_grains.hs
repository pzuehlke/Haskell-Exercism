square :: Integer -> Maybe Integer
square n
    | n <= 0  || 65 <= n    = Nothing
    | otherwise             = Just (2^(n - 1))

total :: Integer
-- total == 1 + 2 + ... + 2^63 = 2^64 - 1
total = 2^64 - 1
