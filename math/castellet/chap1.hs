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

-- Exercise 17

diofantic :: Integer -> Integer -> Integer -> Either String (Integer, Integer)
diofantic a b
  | (mod d c) != 0  = Left "This equation doesn't have solution"
  | c != d          = do
                        (r, s) <- diofantic a b d
                        Right (r * c', s * c')
  | otherwise       = 
  where d = mcd a b
        a' = a `div` d
        b' = b `div` d
        c'  = c `div` d
