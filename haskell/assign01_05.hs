import System.Environment

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg-> Peg -> [Move]
hanoi 0 a b c d = []
hanoi 1 a b c d = [(a, b)]
hanoi n a b c d =
  (hanoi k a c b d) ++
  (hanoi (n-k) a b d c) ++
  (hanoi k c b a d)
  where k = n `quot` 2

main = do x <- getArgs;
          print(hanoi ((read (head x))::Integer) "a" "b" "c" "d")
