tempToC :: Integer -> Float
tempToC degrees = (fromIntegral degrees - 32) / 1.8

tempToF :: Float -> Integer
tempToF degrees = ceiling (1.8 * degrees + 32.0)
