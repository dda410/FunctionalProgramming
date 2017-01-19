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
--removeif :: (a -> Bool) -> [a] -> [a]
--removeif xs = if mod xs 3 == 0 then True else False
nothreefolds :: [Integer]
nothreefolds = filter (\x -> mod x 3 /= 0) naturals

-- Exercise 5
allnfolds :: Integer -> [Integer]
allnfolds y = map (\x -> x * y) naturals

-- Exercise 6
allnaturalsexceptnfolds :: Integer -> [Integer]
allnaturalsexceptnfolds y = filter (\x -> mod x y /= 0) naturals

-- Exercise 7
allelementsexceptnfolds :: Integer -> [Integer] -> [Integer]
allelementsexceptnfolds y xs = filter (\x -> mod x y /= 0) xs

-- Exercise 8
eratosthenes :: [Integer]
eratosthenes =  eratosthenes' (tail naturals)
eratosthenes' (p:xs) = p : eratosthenes' [x | x <- xs, x `mod` p /= 0]

-- Exercise 9
fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)


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

{- Show usage here
   backtointeger (churchnumeral 9)
   backtointeger (churchnumeral (9*2))
   backtointeger (churchnumeral (mod 9 4))
   backtointeger (churchnumeral ((\x -> x * x) 6))
-}

-- Exercise 2
churchequality :: (Eq a, Num a) => ChurchNumeral a -> ChurchNumeral a -> Bool
churchequality x y = backtointeger x == backtointeger y
-- churchequality (churchnumeral 2) (churchnumeral 2) == True
-- churchequality (churchnumeral 2) (churchnumeral 3) == False

-- Exercise 3
successor :: (Num a) => FuncOneArg a
successor cn = (\h -> h . cn h)
-- backtointeger (successor (ch urchnumeral 4)) == 5
-- backtointeger (successor (churchnumeral 5)) == 6

-- Exercise 4
successorb :: (Num a) => FuncOneArg a
successorb cn = (\h -> cn h . h)
-- backtointeger (successorb (churchnumeral 4)) == 5
-- backtointeger (successorb (churchnumeral 5)) == 6

-- Exercise 5
apply1 :: (Eq a, Num a) => FuncOneArg a -> a -> a
apply1 f x = backtointeger (f (churchnumeral x))
-- apply1 successor 5 == 6
-- apply1 successor 6 == 7

-- Exercise 6
addition :: (Num a) => FuncTwoArgs a
addition cn1 cn2 = (\h -> (cn1 h) . (cn2 h))  
-- backtointeger (addition (churchnumeral 1)  (churchnumeral 2)) == 3
-- backtointeger (addition (churchnumeral 5)  (churchnumeral 2)) == 7

multiplication :: (Num a) => FuncTwoArgs a
multiplication cn1 cn2 = (\h -> (cn1 h)) . (\h -> (cn2 h))
-- backtointeger (multiplication  (churchnumeral 2)  (churchnumeral 3)) == 6
-- backtointeger (multiplication  (churchnumeral 2)  (churchnumeral 7)) == 14

-- skip exponentiation !

-- Exercise 7
-- given
-- apply2  that can be used for addition and for multiplication
apply2 f n m = backtointeger (f (churchnumeral n) (churchnumeral m) ) 
-- apply2 addition 2 4 == 6
-- apply2 multiplication 2 4 == 8

-- ---------------------
-- Exercises Binary Trees
-- ---------------------
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

-- Exercise 1
numberofnodes :: BinaryTree a -> Integer
numberofnodes (Leaf) = 0
numberofnodes (Node a b c) = 1 + numberofnodes a + numberofnodes c
-- numberofnodes (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) == 3

-- Exercise 2
height :: BinaryTree a -> Integer
height (Leaf) = 0 
height (Node a b c) = 1 + if height a > height c then height a else height c
-- height (Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf) 4 (Node Leaf 5 Leaf) ) ) == 3

-- Exercise 3
sumnodes :: (Num a) => BinaryTree a -> a
sumnodes (Leaf) = 0
sumnodes (Node a b c) = b + sumnodes a + sumnodes c
-- sumnodes (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) == 6

-- Exercise 4
mirror :: BinaryTree a -> BinaryTree a
mirror (Leaf) = Leaf
mirror (Node a b c) = Node (mirror c) b (mirror a)
-- mirror (Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf) 4 (Node Leaf 5 Leaf) ) ) == Node (Node (Node Leaf 5 Leaf) 4 (Node Leaf 3 Leaf)) 2 (Node Leaf 1 Leaf)

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
