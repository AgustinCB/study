-- Exercise 16

mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a 1 = 1
mcd a b
  | b > a     = mcd b a
  | a == b    = a
  | otherwise = mcd b (mod a b)

mcm :: Integer -> Integer -> Integer
mcm a b = (a * b) `div` (mcd a b)
