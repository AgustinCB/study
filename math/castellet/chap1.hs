import Data.List (transpose)

-- Exercise 16

mcd :: Integer -> Integer -> Integer
mcd a b = fst $ mcd' a b []

mcd' :: Integer -> Integer -> [Integer] -> (Integer, [Integer])
mcd' a 0 acc = (a, acc)
mcd' a 1 acc = (1, acc)
mcd' a b acc
  | b > a     = mcd' b a acc
  | a == b    = (a, acc)
  | otherwise = mcd' b (mod a b) ((div a b) : acc)

mcm :: Integer -> Integer -> Integer
mcm a b = (a * b) `div` (mcd a b)

-- Exercise 17

mmult :: Num a => [[a]] -> [[a]] -> [[a]] 
mmult a b = [ [ sum $ zipWith (*) ar bc | bc <- (transpose b) ] | ar <- a ]

numToEuclideMatrix :: Num a => a -> [[a]]
numToEuclideMatrix a = [[0, 1], [1, a]]

ident = [[1, 0], [0, 1]]

diofantic :: Integer -> Integer -> Integer -> Either String (Integer, Integer)
diofantic a b c
  | (mod d c) /= 0  = Left "This equation doesn't have solution"
  | c /= d          = do
                        (r, s) <- diofantic a b d
                        Right (r * c', s * c')
  | otherwise       = let r = qMatrixesMultiplied !! 0
                      in Right $ (r !! 0, r !! 1)
  where (d, qs) = mcd' a b []
        qMatrixes = map (numToEuclideMatrix . negate) qs
        qMatrixesMultiplied = foldr mmult ident qMatrixes
        a' = a `div` d
        b' = b `div` d
        c'  = c `div` d

-- Exercise 18
factorize :: Integer -> [Integer]
factorize number = filter (isFactor number) tries
  where tries = [1..limit]
        limit = number `div` 2
        isFactor :: Integer -> Integer -> Bool
        isFactor number factor = (isPrime factor) && ((mod number factor) == 0)

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime 3 = True
isPrime n = foldr diffOne True r
  where k = 10
        r = [2..(min (n-2) (k+2))]
        diffOne :: Integer -> Bool -> Bool
        diffOne a acc = ((mod (a^(n-1)) n) == 1) && acc
