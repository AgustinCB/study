import Test.QuickCheck

import Cipher (caesar, unCaesar)

data CaesarInput = CaesarInput Integer String deriving (Show)

caesarInputGen :: Gen CaesarInput
caesarInputGen = do
  n <- arbitrary
  s <- sublistOf $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ [' ', '+', '-', '/','=','*','_']
  return (CaesarInput n s)

inverseCaesar :: CaesarInput -> Bool
inverseCaesar (CaesarInput n s) = (unCaesar n (caesar n s)) == s

main :: IO()
main = quickCheck $ forAll caesarInputGen inverseCaesar
