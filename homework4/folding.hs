{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}

fun1 :: [Integer] -> Integer
fun1 = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 =  sum . filter even . takeWhile (/= 1) . iterate f
   where f n = if even n then n `div` 2 else 3 * n + 1

data Tree a = Leaf
   | Node Integer (Tree a) a (Tree a)
   deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
{-
if entry = 'A'..'Z' = 'A':('B':(...:('Z':[])))
same as :
   insert 'A' (insert 'B' (... (insert 'Z' Leaf)))
-}

insert :: a -> Tree a -> Tree a
insert x Leaf              = Node 0 Leaf x Leaf
insert x (Node _ l curr r) =
   Node newHeight newLeft curr newRight
   where newLeft = if height l <= height r then insert x l else l
         newRight = if height l > height r then insert x r else r
         newHeight = 1 + max (height newLeft) (height newRight)

height :: Tree a -> Integer
height Leaf           = 0
height (Node x _ _ _) = x

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x:xs) []