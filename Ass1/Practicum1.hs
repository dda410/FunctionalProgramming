



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
myappend = undefined

-- Exercise 12
myreverse :: [a] -> [a]
myreverse = undefined

-- Exercise 13
mymember :: (Eq a) => a -> [a] -> Bool
mymember = undefined

-- Exercise 14
mysquaresum :: [Integer] -> Integer
mysquaresum = undefined

-- Exercise 15
range :: Integer -> Integer -> [Integer]
range = undefined

-- Exercise 16
myconcat :: [[a]] -> [a]
myconcat = undefined

-- Exercise 17
insert :: Ord a => a -> [a] -> [a]
insert = undefined

insertionsort :: Ord a => [a] -> [a]
insertionsort = undefined

-- Exercise 18
quicksort :: Ord a => [a] -> [a]
quicksort = undefined

-- Exercise 19
evensB :: [Integer] -> [Integer]
evensB = undefined

-- Exercise 20
mymap :: (a -> b) -> [a] -> [b]
mymap = undefined

-- Exercise 21
twice :: (a -> a) -> a -> a
twice = undefined

-- Exercise 22
compose :: (b -> c) -> (a -> b) -> a -> c
compose = undefined

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
