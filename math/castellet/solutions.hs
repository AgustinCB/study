module Castellet where

import Control.Applicative (liftA2)
import Data.List (find, transpose)

-- Chapter 1

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

-- Exercise 19
primes = 2 : 3 : 5 : filter isPrime [6..]

-- Exercise 20
modMult :: Integer -> Integer -> Integer -> Integer
modMult p n i = mod (n * i) p

modAdd :: Integer -> Integer -> Integer -> Integer
modAdd p n i = mod (n + i) p

modSub :: Integer -> Integer -> Integer -> Integer
modSub p n i = mod (n - i) p

inverseOn :: Integer -> Integer -> Either String Integer
inverseOn n p
  | not $ isPrime p = Left "The module should be prime"
  | n == 0          = Left "0 doesn't have inverse on Z/(p)"
  | otherwise       = Right $ head $ filter (\i -> ((modMult p n i) == 1)) [1..(p-1)]

-- Exercise 21
squareRootOn :: Integer -> Integer -> Either String Integer
squareRootOn n p
  | not $ isPrime p = Left "The module should be prime"
  | n == 0          = Left "0 doesn't have inverse on Z/(p)"
  | otherwise       = Right $ head $ filter (\i -> ((modMult p i i) == n)) [1..(p-1)]

-- Chapter 2

-- Ex 1
quadraticOn :: Integer -> Integer -> Integer -> Integer -> Either String Integer
quadraticOn p a b c
  | not $ isPrime p = Left "The module should be prime"
  | a == 0          = fmap (modMult p (-c)) $ inverseOn b p
  | otherwise       = dn >>= (\d -> fmap (modMult p d) (inverseOn (modMult p 2 a) p))
  where bsquare = modMult p b b
        rootContent :: Either String Integer
        rootContent = squareRootOn (modSub p bsquare $ modMult p c (modMult p 4 a)) p
        dn :: Either String Integer
        dn = fmap (modSub p (modMult p (-1) b)) rootContent

-- Ex 2
trimmedPol :: [Integer] -> [Integer]
trimmedPol = dropWhile (== 0)

normalizedOn :: Integer -> [Integer] -> [Integer]
normalizedOn p = fmap (\n -> mod n p)

modMultPol :: Integer -> [Integer] -> Integer -> [Integer]
modMultPol p pl m = fmap (modMult p m) pl

xs :: Floating m => m -> [m]
xs x = fmap ((**) x) $ fmap fromIntegral [0..]

applyPol :: Integer -> Integer -> [Integer] -> Integer
applyPol p x pol = sum $ normalizedOn p $ fmap (uncurry (*)) $ zip pol $ fmap floor (xs fx)
  where fx :: Float
        fx = fromIntegral x

modSubPol :: Integer -> [Integer] -> [Integer] -> [Integer]
modSubPol p a b
  | length a < length b  = modSubPol p (leadingZeros a b) b
  | length b < length a  = modSubPol p a (leadingZeros b a)
  | length a == length b = zipWith (-) a b
  where leadingZeros :: [Integer] -> [Integer] -> [Integer]
        leadingZeros a b = (map (const 0) [0..(length b - length a - 1)]) ++ a

divPoly :: Integer -> [Integer] -> [Integer] -> Either String ([Integer], [Integer])
divPoly p a b
  | not $ isPrime p                    = Left "The module should be prime"
  | length trimmedA < length trimmedB  = Right ([0], a)
  | length trimmedA == length trimmedB = liftA2 (,) den newRest
  | length trimmedA > length trimmedB  = let nr = newRest >>= (\nr -> divPoly p nr b)
                                         in liftA2 (\d -> \r -> (d ++ (fst r), snd r)) den nr
  where trimmedA = normalizedOn p $ trimmedPol a
        trimmedB = normalizedOn p $ trimmedPol b
        den = fmap (\n -> [modMult p (trimmedA !! 0) n]) $ inverseOn (trimmedB !! 0) p
        newRest = fmap (\d -> trimmedPol (modSubPol p trimmedA (modMultPol p trimmedB (d !! 0)))) den

getDivisors :: Integer -> [Integer] -> Either String [[Integer]]
getDivisors p pol
  | length trimmedP == 0 = Right []
  | length trimmedP == 1 = Right [trimmedP]
  | otherwise              = case nextDiv of Just div -> fmap ((++) [buildRoot div]) ((divPoly p pol (buildRoot div)) >>= (\r -> (getDivisors p $ fst r)))
                                             Nothing -> Right [trimmedP]
  where trimmedP :: [Integer]
        trimmedP = normalizedOn p $ trimmedPol pol
        buildRoot :: Integer -> [Integer]
        buildRoot n = [1, n]
        nextDiv = find (\e -> mod (applyPol p e trimmedP) p == 0) [0..p]

getAllFactors :: (Integer, Integer) -> [(Integer, Integer)]
getAllFactors (a, b) = [(n,d) | n <- numerators,
                                d <- denominators]
  where numerators = factorize a
        denominators = factorize b

apply :: Floating m => m -> [m] -> m
apply x pol = sum $ fmap (uncurry (*)) $ zip pol $ xs x

zeros :: [Integer] -> [Float]
zeros p
  | length p == 0 = []
  | length p == 1 = [fromIntegral (-f)]
  | otherwise     = filter (\v -> 0 == apply v (fmap fromIntegral p)) results
  where f = p !! 0
        l = last p
        possibleZeros = getAllFactors (f,l)
        results :: [Float]
        results = fmap (uncurry (\i1 -> \i2 -> (fromIntegral i1) / (fromIntegral i2))) possibleZeros
