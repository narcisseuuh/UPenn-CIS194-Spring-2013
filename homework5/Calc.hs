{-# LANGUAGE TypeSynonymInstances #-}

module Calc where
   import ExprT
   import StackVM (stackVM, Program, StackVal(..), StackExp(PushI, PushB))
   import Parser (parseExp)
   import qualified StackVM
   import qualified Data.Map as M

   eval :: ExprT -> Integer
   eval e =
      case e of
         Lit x     -> x
         Mul e1 e2 -> eval e1 * eval e2
         Add e1 e2 -> eval e1 + eval e2

   evalStr :: String -> Maybe Integer
   evalStr s =
      let parsed = parseExp Lit Add Mul s in
      case parsed of
         Nothing -> Nothing
         Just e  -> Just (eval e)

   class Expr a where
      lit :: Integer -> a
      add :: a -> a -> a
      mul :: a -> a -> a

   instance Expr ExprT where
      lit = Lit
      add = Add
      mul = Mul

   newtype MinMax = MinMax Integer deriving (Eq, Show)
   newtype Mod7 = Mod7 Integer deriving (Eq, Show)

   instance Expr Integer where
      lit = id
      add = (+)
      mul = (*)

   instance Expr Bool where
      lit x = x > 0
      add = (||)
      mul = (&&)

   instance Expr MinMax where
      lit = MinMax
      add (MinMax x) (MinMax y) = MinMax $ max x y
      mul (MinMax x) (MinMax y) = MinMax $ min x y

   instance Expr Mod7 where
      lit x = Mod7 (x `mod` 7)
      add (Mod7 t) (Mod7 z) = Mod7 $ (t + z) `mod` 7
      mul (Mod7 t) (Mod7 z) = Mod7 $ (t * z) `mod` 7

   testExp :: Expr a => Maybe a
   testExp = parseExp lit add mul "(3 * -4) + 5"

   instance Expr Program where
      lit x = [PushI x]
      -- x and y are of type Program
      add x y =
         case (stackVM x, stackVM y) of
            (Left _, _)        -> x -- throwing the error associated to first operation
            (_, Left _)        -> y -- throwing the error associated to second operation
            (Right u, Right v) -> valToProgram u ++ valToProgram v ++ [StackVM.Add]

      mul x y =
         case (stackVM x, stackVM y) of
            (Left _, _)        -> x
            (_, Left _)        -> y
            (Right u, Right v) -> valToProgram u ++ valToProgram v ++ [StackVM.Mul]

   valToProgram :: StackVal -> Program
   valToProgram x =
      case x of
         IVal i -> [PushI i]
         BVal b -> [PushB b]
         Void   -> []


   compile :: String -> Maybe Program
   compile = parseExp lit add mul