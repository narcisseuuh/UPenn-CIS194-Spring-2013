module Golf where
   import qualified Control.Monad

   filtering :: Int -> [a] -> Int -> [a]
   filtering every list n =
      case list of
         []     -> []
         (x:xs) ->
            if n `mod` every == 0 then x:filtering every xs 1
            else filtering every xs (n+1)

   toFilter :: Int -> [a] -> [a]
   toFilter x l =
      filtering x l 1


   skips :: [a] -> [[a]]
   skips list =
      let n = length list in
      [toFilter x list | x <- [1..n]]

   localMaximaKnowing :: [Integer] -> Integer -> [Integer]
   localMaximaKnowing list preced =
      case list of
         []       -> []
         [x]      -> []
         (x:y:xs) ->
            if x > preced && x > y then x:localMaximaKnowing (y:xs) x
            else localMaximaKnowing (y:xs) x


   localMaxima :: [Integer] -> [Integer]
   localMaxima list =
      case list of
         []       -> []
         [x]      -> [x]
         (x:y:xs) ->
            if x > y then x:localMaximaKnowing (y:xs) x
            else localMaximaKnowing (y:xs) x

   generateLine :: Integer -> [Integer] -> String
   generateLine i occurrences =
      case occurrences of
         []     -> "\n"
         (x:xs) -> if x >= i then "*" ++ generateLine i xs else " " ++ generateLine i xs

   count :: Integer -> [Integer] -> Integer
   count i list =
      case list of
         []     -> 0
         (x:xs) ->
            if x == i then 1 + count i xs
            else count i xs


   histogram :: [Integer] -> String
   histogram list =
      let occurrences = [count i list | i <- [1..9]] in
      let m = maximum occurrences in
      let lines = [generateLine i occurrences | i <- [1..m]] in
      Control.Monad.join (reverse lines) ++ "==========\n0123456789\n"