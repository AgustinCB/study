import System.Environment

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n = (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:(y:zs)) = x : y*2 : doubleEveryOther(zs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum(toDigits x) + sumDigits(xs)

validate :: Integer -> Bool
validate n = sumDigits(doubleEveryOther(toDigitsRev n)) `mod` 10 == 0

main = do x <- getArgs;
          print (validate(read(head x) :: Integer))
