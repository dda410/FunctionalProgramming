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

-- The disks are numbered as follows: the one at the top will be disk number 1 (the smallest one)
-- the one at the bottom will be disk number n (where n is the total number of disks)
-- the other disks will be numbered increasingly from 1 to n
type Rod = String
type Move = (Integer, Rod, Rod)
hanoi :: Integer -> Rod -> Rod -> Rod -> [Move]
hanoi 0 _ _ _ = []
hanoi n start end temp =
      let nMinusOne = subtract 1 n
          diskno = nMinusOne + 1
              in hanoi nMinusOne start temp end ++
                 [(diskno, start, end)] ++
                               hanoi nMinusOne temp end start
-- hanoi 3 "a" "b" "c" == [(1,"a","b"),(2,"a","c"),(1,"b","c"),(3,"a","b"),(1,"c","a"),(2,"c","b"),(1,"a","b")]
-- An ascii art example of the previous command execution:
--
--  start:           end:
--
--  A   B   C        A   B   C
--  +   +   +        +   +   +
--  |   |   |        |   |   |
--  |   |   |        |   |   |
--  1   |   |        |   1   |
--  2   |   |        |   2   |
--  3   |   |        |   3   |
--  +   +   +        +   +   +

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
successor = (\x s z -> s (x s z))
-- backtointeger (successor (churchnumeral 4)) == 5
-- backtointeger (successor (churchnumeral 5)) == 6

-- Exercise 4
successorb :: (Num a) => FuncOneArg a
successorb = (\x s z -> x s (s z))
--successorc cn = (\h -> h . cn h)
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
-- In-order traversal: from smaller to bigger
flatten :: BinaryTree a -> [a]
flatten (Leaf) = []
flatten (Node a b c) = (flatten a) ++ [b] ++ (flatten c)
-- flatten (Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf) 4 (Node Leaf 5 Leaf) ) ) == [1,2,3,4,5]

-- Exercise 6
treemap :: (a -> b) -> BinaryTree a -> BinaryTree b
treemap f (Leaf) = Leaf
treemap f (Node a b c) = Node (treemap f a) (f b) (treemap f c) 
-- treemap (\x -> x + 1) (Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf) 4 (Node Leaf 5 Leaf) ) ) == Node (Node Leaf 2 Leaf) 3 (Node (Node Leaf 4 Leaf) 5 (Node Leaf 6 Leaf))

-- -------------------------
-- Exercises Binary Search Trees
-- -------------------------

-- Exercise 1
smallerthan :: (Ord a) => a -> BinaryTree a -> Bool
smallerthan x (Leaf) = True
smallerthan x (Node a b c) =
  if x > b
  then (smallerthan x c)
  else False

largerthan :: (Ord a) => a -> BinaryTree a -> Bool
largerthan x (Leaf) = True
largerthan x (Node a b c) =
  if x < b
  then (largerthan x a)
  else False

-- Exercise 2
isbinarysearchtree :: (Ord a) => BinaryTree a -> Bool
isbinarysearchtree (Leaf) = True
isbinarysearchtree (Node a b c) = ((smallerthan b a) && (largerthan b c) && (isbinarysearchtree a) && (isbinarysearchtree c))

-- Exercise 3
iselement :: (Ord a, Eq a) => a -> BinaryTree a -> Bool
iselement x (Leaf) = False
iselement x (Node a b c) =
  if x == b
  then True
  else if x < b
  then (iselement x a)
  else (iselement x c)

-- Exercise 4
insert :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
insert x (Leaf) = (Node Leaf x Leaf)
insert x (Node a b c) =
  if x == b
  then (Node a b c)
  else if x < b
  then (Node (insert x a) b (c))
  else (Node (a) b (insert x c))

-- Exercise 5
createbinarysearchtree :: (Ord a, Eq a) => [a] -> BinaryTree a
createbinarysearchtree [] = error "cannot build BST from empty list"
createbinarysearchtree (h:[]) = (Node Leaf h Leaf)
createbinarysearchtree (h:t) = createbinarysearchtree' t (Node Leaf h Leaf)
createbinarysearchtree' :: (Ord a, Eq a) => [a] -> BinaryTree a -> BinaryTree a
createbinarysearchtree' (h:[]) (Node a b c) = insert h (Node a b c)
createbinarysearchtree' (h:t) (Node a b c) = createbinarysearchtree' t (insert h (Node a b c))

-- Exercise 6
remove :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
remove x (Leaf) = Leaf
remove x (Node a b c) =
  if x < b
  then (Node (remove x a) b c)
  else if x > b
  then (Node a b (remove x c))
  else if (a == Leaf) && (c == Leaf)
  then Leaf
  else if a == Leaf
  then c
  else if c == Leaf
  then a
  else
    let minelement = value (smallest (Node a b c))
          in (Node (remove minelement a) minelement c)
-- remove 3 (Node (Node (Node Leaf 2 Leaf) 3 (Node Leaf 4 Leaf) ) 5 (Node (Node Leaf 6 Leaf) 8 (Node Leaf 10 Leaf) ) ) == Node (Node Leaf 2 (Node Leaf 4 Leaf)) 5 (Node (Node Leaf 6 Leaf) 8 (Node Leaf 10 Leaf))
-- remove 2 (Node (Node (Node Leaf 2 Leaf) 3 (Node Leaf 4 Leaf) ) 5 (Node (Node Leaf 6 Leaf) 8 (Node Leaf 10 Leaf) ) ) == Node (Node Leaf 3 (Node Leaf 4 Leaf)) 5 (Node (Node Leaf 6 Leaf) 8 (Node Leaf 10 Leaf)) 

-- value returns the value of a node
value (Node a b c) = b

-- smallest returns the smallest node of a tree
smallest :: (Ord a, Eq a) => BinaryTree a -> BinaryTree a
smallest (Leaf) = Leaf 
smallest (Node a b c) =
  if smallest a /= Leaf
  then smallest a
  else (Node Leaf b Leaf)
  
