module Practicum2A where

{-
Name:           <Name and family name>
VU-net id:      <VU-net id>
Student number: <Student number>
Discussed with: <In case you discussed the exercises with someone else,
                 please mention his/her name(s) explicitly here>
Remarks:        <In case something need special attention,
                 please tell us>
Sources:        <in case you used sources such as books or webpages
                 please mention them here>
-}

----------------------------
-- Exercise Tower of Hanoi
----------------------------

type Rod = String
type Move = (Integer, Rod, Rod)
hanoi :: Integer -> Rod -> Rod -> Rod -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 start end _ = [(1, start, end)]
hanoi n start end temp =
      let nMinusOne = subtract 1 n
              in hanoi nMinusOne start temp end ++
                        hanoi 1 start end temp ++
                               hanoi nMinusOne temp end start


-- -------------------------
-- Exercises Infinite Lists
-- -------------------------

-- Exercise 1
naturals :: [Integer]
naturals = naturals' 1
naturals' x = x : naturals' (x+1)

-- Exercise 2
zeroesandones :: [Integer]
zeroesandones = zeroesandones' True
zeroesandones' b = if b then 0 : zeroesandones' False else 1 : zeroesandones' True

-- Exercise 3
threefolds :: [Integer]
threefolds = map (\x -> x * 3) naturals

-- Exercise 4
removeif :: (a -> Bool) -> [a] -> [a]
removeif = undefined

nothreefolds :: [Integer]
nothreefolds = undefined

-- Exercise 5
allnfolds :: Integer -> [Integer]
allnfolds = undefined

-- Exercise 6
allnaturalsexceptnfolds :: Integer -> [Integer]
allnaturalsexceptnfolds = undefined

-- Exercise 7
allelementsexceptnfolds :: Integer -> [Integer] -> [Integer]
allelementsexceptnfolds = undefined

-- Exercise 8
eratosthenes :: [Integer]
eratosthenes = undefined

-- Exercise 9
fibonacci :: [Integer]
fibonacci = undefined

-- -----------------------
-- Exercise Church Numerals
-- -----------------------

type ChurchNumeral a = (a -> a) -> a -> a
type FuncOneArg  a = ChurchNumeral a -> ChurchNumeral a
type FuncTwoArgs a = ChurchNumeral a -> ChurchNumeral a -> ChurchNumeral a

-- Exercise 1
churchnumeral :: (Eq a, Num a) => a -> ChurchNumeral a
churchnumeral n =
  if   n == 0
  then \s z -> z
  else \s z -> churchnumeral (n - 1) s (s z)

backtointeger :: (Num a) => ChurchNumeral a -> a
backtointeger cn = cn (+1) 0

{- Show usage here -}

-- Exercise 2
churchequality :: (Eq a, Num a) => ChurchNumeral a -> ChurchNumeral a -> Bool
churchequality = undefined

-- Exercise 3
successor :: (Num a) => FuncOneArg a
successor = undefined

-- Exercise 4
successorb :: (Num a) => FuncOneArg a
successorb = undefined

-- Exercise 5
apply1 :: (Eq a, Num a) => FuncOneArg a -> a -> a
apply1 = undefined

-- Exercise 6
addition :: (Num a) => FuncTwoArgs a
addition = undefined

multiplication :: (Num a) => FuncTwoArgs a
multiplication = undefined

-- skip exponentiation !

-- Exercise 7
-- given
-- apply2  that can be used for addition and for multiplication
apply2 f n m = backtointeger (f (churchnumeral n) (churchnumeral m) ) 

-- ---------------------
-- Exercises Binary Trees
-- ---------------------
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

-- Exercise 1
numberofnodes :: BinaryTree a -> Integer
numberofnodes = undefined

-- Exercise 2
height :: BinaryTree a -> Integer
height = undefined

-- Exercise 3
sumnodes :: (Num a) => BinaryTree a -> a
sumnodes = undefined

-- Exercise 4
mirror :: BinaryTree a -> BinaryTree a
mirror = undefined

-- Exercise 5
flatten :: BinaryTree a -> [a]
flatten = undefined

-- Exercise 6
treemap :: (a -> b) -> BinaryTree a -> BinaryTree b
treemap = undefined

-- -------------------------
-- Exercises Binary Search Trees
-- -------------------------

-- Exercise 1
smallerthan :: (Ord a) => a -> BinaryTree a -> Bool
smallerthan = undefined

largerthan :: (Ord a) => a -> BinaryTree a -> Bool
largerthan = undefined

-- Exercise 2
isbinarysearchtree :: (Ord a) => BinaryTree a -> Bool
isbinarysearchtree = undefined

-- Exercise 3
iselement :: (Ord a, Eq a) => a -> BinaryTree a -> Bool
iselement = undefined

-- Exercise 4
insert :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
insert = undefined

-- Exercise 5
createbinarysearchtree :: (Ord a, Eq a) => [a] -> BinaryTree a
createbinarysearchtree = undefined

-- Exercise 6
remove :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
remove = undefined
