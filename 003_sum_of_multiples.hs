module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples []       n = 0
sumOfMultiples [m]      n
  | m == 0      = 0
  | m > 0       = m * (d + 1) * d `div` 2
  | otherwise   = sumOfMultiples [-m] n 
    where
      d = (n - 1) `div` m
sumOfMultiples (m:ms)   n = sumOfMultiples ms n + 
                            sumOfMultiples [m] n -
                            sumOfMultiples rs n
                              where
                                rs = map (lcm m) ms
