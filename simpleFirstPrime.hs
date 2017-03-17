
firstPrime  :: Integer -> Integer
firstPrime' :: Integer -> Integer
prime       :: Integer -> Integer -> Bool

firstPrime n
  | n <= 0 = error "Firstprime: Non-positive Argument"
  | mod n 10 == 0 = firstPrime' $ n + 1
  | otherwise = firstPrime' $ n + 11 -  mod n 10


firstPrime' n
  | prime 3 n = n
  | otherwise = firstPrime' $ n + 10

prime x n
  | x * x > n = True
  | mod n x == 0 = False
  | otherwise = prime (x + 2) n
