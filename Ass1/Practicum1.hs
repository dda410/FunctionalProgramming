



module Practicum1 where

{-
Name:           <Name and family name>
VU-net id:      <dda410>
Student number: <2553339>
Discussed with: <In case you discussed the exercises with someone else,
                 please mention his/her name(s) explicitly here>
		 Remarks:        <In case something need special attention,
		                  please tell us>
				  Sources:        <in case you used sources such as books or webpages
				                   please mention them here>
						   -}

-- Below you will find templates for the exercises. For each exercise,
-- replace 'undefined' by your definition and supply at least two different
-- meaningful tests to show that your code produces sane results. The
-- tests may be given in comments (see exercise 1).

-- Exercise 1
maxi :: Integer -> Integer -> Integer
maxi x y = if x > y
              then x
              else y             
-- maxi 2 3 == 3
-- maxi 8 0 == 8

-- Exercise 2
fourAscending :: Integer -> Integer -> Integer -> Integer -> Bool
fourAscending x y z w = (y > x) && (z > y) && (w > z)
-- fourAscending 1 2 3 5 == True
-- fourAscending 1 2 3 3 == False

-- Exercise 3
fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual x y z w = (x == y) && (y == z) && (z == w)
-- fourEqual 2 2 2 2 == True
-- fourEqual 2 2 2 3 == False

-- Exercise 4
fourDifferent :: Integer -> Integer -> Integer -> Integer -> Bool
fourDifferent x y z w = (x /= y) && (x /= z) && (x /= w) && (y /= z) && (y /= w) && (z /= w)
-- fourDifferent 2 3 4 5 == True
-- fourDifferent 2 3 4 4 == False 

-- Exercise 5
{-
<threeDifferent 1 2 1 == True>
   -}

-- Exercise 6
factorial :: Integer -> Integer
factorial x = if x == 1 then x else x * factorial (x - 1)
-- factorial 4 == 24
-- factorial 5 == 120

-- Exercise 7
fib :: Integer -> Integer
-- fib x = if x == 0 || x == 1 then x else fib (x - 1) + fib (x - 2)
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-- fib 10 == 55
-- fib 14 == 377

------------------------------------------------------------
-- Exercise 8
-- it is possible to define auxiliary functions
strangeSummation :: Integer -> Integer
strangeSummation = undefined

-- Exercise 9
lengthList :: [Integer] -> Integer
lengthList []  = 0
lengthList (h:t) = 1 + lengthList t

--lengthListAlternative :: [Integer] -> Integer
--lengthListAlternative l =
--  case l of
--      [] -> 0
--          (h:t) -> 1 + (lengthListAlternative t)

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (h:t) = h + sumList t
-- sumList [1, 2, 3] == 6
-- sumList [1, 2, 3, 10] == 16

-- Exercise 10
doubleList :: [Integer] -> [Integer]
doubleList [] = []
doubleList (h:t) = (h * 2) : doubleList t
-- doubleList [1, 2] == [2,4]
-- doubleList [1, 4] == [2,8]

-- Exercise 11
myappend :: [a] -> [a] -> [a]
myappend []  ys = ys
myappend (x:xs) ys = x : myappend xs ys
--  myappend [1,2,3] [4,5,6] == [1,2,3,4,5,6]
--  myappend [4,5,6] [1,2,3] == [4,5,6,1,2,3]

-- Exercise 12
myreverse :: [a] -> [a]
myreverse [] = []
myreverse t = last t : myreverse (init t)
-- myreverse [1,2,3,4,5,6] == [6,5,4,3,2,1]
-- myreverse [6,5,4,3,2,1] == [1,2,3,4,5,6]

-- Exercise 13
mymember :: (Eq a) => a -> [a] -> Bool
mymember xs [] = False
mymember xs (ys:y) = (xs == ys) || mymember xs y
-- mymember 2 [1,2,3] == True
-- mymember 2 [1,0,3] == False

-- Exercise 14
mysquaresum :: [Integer] -> Integer
mysquaresum [] = 0
mysquaresum t = head t * head t + mysquaresum (tail t)
-- mysquaresum [2, 2, 3] == 17
-- mysquaresum [2, 2, 3, 4] == 33

-- Exercise 15
range :: Integer -> Integer -> [Integer]
range x y = if x > y then [] else if x == y then y:[] else x : range (x+1) y
-- range 10 2  == []
-- range 2 10 == [2,3,4,5,6,7,8,9,10]

-- Exercise 16
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat x =  x !! 0 ++ (myconcat (tail x))

-- Exercise 17
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x < y then x:y:ys else y : insert x ys

insertionsort :: Ord a => [a] -> [a]
insertionsort [x] = [x]
insertionsort (x:xs) = insert x (insertionsort xs)

-- Exercise 18
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = quicksort (filter (\x -> x < p) xs) ++ [p] ++ (quicksort (filter (\x -> x > p) xs) )

-- Exercise 19
evensB :: [Integer] -> [Integer]
evensB ys  = [x | x<- ys, even x]

-- Exercise 20
mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = f x : mymap f xs
-- mymap (\x -> succ x) [1,2,3] == [2,3,4]
-- mymap (\x -> x*x) [1,2,3] == [1,4,9]

-- Exercise 21
twice :: (a -> a) -> a -> a
twice f x = f (f x)
-- twice (\x -> x*x) 2  == 16
-- twice (\x -> x-2) 2 == -2

-- Exercise 22
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)
-- compose (\x -> x*x) (\x -> x+4) 2 == 36
-- compose (\x -> x*x*x) (\x -> x -1) 4 == 27

-- Exercise 23
mylast :: [a] -> a
mylast = undefined

-- Exercise 24
mylastb :: [a] -> a
mylastb = undefined

-- Exercise 25
myinit, myinitb :: [a] -> [a]
myinit = undefined
myinitb = undefined

-- Exercise 26
mysecondconcat :: [[a]] -> [a]
mysecondconcat = undefined

mysecondreverse :: [a] -> [a]
mysecondreverse = undefined

-- Exercise 27
prefix :: [a] -> [[a]]
prefix = undefined
