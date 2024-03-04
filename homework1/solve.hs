reverseList :: [Integer] -> [Integer]
reverseList list =
   case list of
      [] -> []
      x:xs -> reverseList xs ++ [x]


-- Exercise 1

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
   | x <= 0    = []
   | otherwise = let integer = x `mod` 10
      in (integer:q)
         where q = toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits x = reverseList (toDigitsRev x)


-- Exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list =
   let reverse = reverseList list in
   reverseList (doubleOther reverse False) where
      doubleOther list isPair =
         case list of
            []   -> []
            x:xs -> if isPair then 2*x:doubleOther xs False else x:doubleOther xs True


-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits list = sum (concatMap toDigitsRev list)

-- Exercise 4

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits) x `mod` 10 == 0

-- Exercise 5

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c =
   case n of
      1 -> [(a,b)]
      _ -> hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a


-- main
main :: IO()
main = print "Hello world!"