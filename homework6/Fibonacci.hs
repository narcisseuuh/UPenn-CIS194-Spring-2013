{-# OPTIONS_GHC -Wno-missing-methods #-}
module Fibonacci where
   import Data.List
   fib :: Integer -> Integer
   fib n =
      case n of
         0 -> 0
         1 -> 1
         _ -> fib (n-1) + fib (n-2)

   fibs1 :: [Integer]
   fibs1 = [fib i | i <- [0..]]

   fibs2 :: [Integer]
   fibs2 = map fst . scanl' (\(x, y) _ -> (y, x+y)) (0,1) $ [0..]

   data Stream a = Cons a (Stream a)

   streamToList :: Stream a -> [a]
   streamToList (Cons x xs) = x:streamToList xs

   instance Show a => Show (Stream a) where
      show = show . take 20 . streamToList

   streamRepeat :: a -> Stream a
   streamRepeat x = Cons x (streamRepeat x)

   streamMap :: (a -> b) -> Stream a -> Stream b
   streamMap f (Cons x st) = Cons (f x) (streamMap f st)

   streamFromSeed :: (a -> a) -> a -> Stream a
   streamFromSeed f x = Cons x (streamFromSeed f (f x))

   nats :: Stream Integer
   nats = streamFromSeed succ 0

   maxTab :: [Integer] -> Integer
   maxTab = foldr max (error "no maximum")

   ruler :: Stream Integer
   ruler = streamMap (\x -> maxTab [n | n <- [0..], (2^n) `mod` x == 0]) nats

   x :: Stream Integer
   x = Cons 0 (Cons 1 (streamRepeat 0))

   instance Num (Stream Integer) where
      fromInteger :: Integer -> Stream Integer
      fromInteger x = Cons x (streamRepeat 0)

      negate :: Stream Integer -> Stream Integer
      negate = streamMap negate

      (+) :: Stream Integer -> Stream Integer -> Stream Integer
      (+) (Cons x xs) (Cons y ys) = Cons (x+y) (xs + ys)

      (*) :: Stream Integer -> Stream Integer -> Stream Integer
      (*) (Cons a0 aPrime) (Cons b0 bPrime) =
         let tl = streamMap (*a0) bPrime + aPrime * Cons b0 bPrime in
         Cons (a0 * b0) tl
   
   instance Fractional (Stream Integer) where
      (/) :: Stream Integer -> Stream Integer -> Stream Integer
      (/) (Cons a0 aPrime) (Cons b0 bPrime) = q
         where q = Cons (a0 `div` b0) $ streamMap (`div` b0) (aPrime - q*bPrime)

   fibs3 :: Stream Integer
   fibs3 :: Stream Integer =
      x / (1 - x - x^2)

   data Matrix = Matrix Integer Integer Integer Integer

   instance Num Matrix where
      fromInteger :: Integer -> Matrix
      fromInteger n = Matrix n 0 0 n

      (*) :: Matrix -> Matrix -> Matrix
      (*) (Matrix a0 a1 a2 a3) (Matrix b0 b1 b2 b3) =
         Matrix (a0*b0 + a1*b2) (a0*b1 + a1*b3) (a2*b0 + a3*b2) (a2*b1 + a3*b3)
   
   fib4 :: Integer -> Integer
   fib4 n =
      let m = Matrix 1 1 1 0 in
      case m^n of
         Matrix _ res _ _ -> res