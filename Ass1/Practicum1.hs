



module Practicum1 where

{-
Name:           <Dimitri Diomaiuta>
VU-net id:      <dda410>
Student number: <2553339>
Discussed with: <In case you discussed the exercises with someone else,
                 please mention his/her name(s) explicitly here>
		 Remarks:        <In case something need special attention,
		                  please tell us>
				  Sources:        <I used the following webpages as main sources of programming
                                                   tutorials:
                                                     - http://learnyouahaskell.com/chapters
                                                     - https://www.haskell.org/tutorial/
				                   >
						   -}

-- Below you will find templates for the exercises. For each exercise,
-- replace 'undefined' by your definition and supply at least two different
-- meaningful tests to show that your code produces sane results. The
-- tests may be given in comments (see exercise 1).

-- Exercise 1
maxi :: Integer -> Integer -> Integer
maxi x y = if x > y then x else y             
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
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-- fib 10 == 55
-- fib 14 == 377

-- Exercise 8
-- it is possible to define auxiliary functions
helper :: Integer -> Integer -> Integer
helper x y = if x /= (y + 1) then x + helper (x+1) y else 0
strangeSummation :: Integer -> Integer
strangeSummation x = helper x (x+7)
-- strangeSummation 4 == 60 (that is 4+5+6+7+8+9+10+11)
-- strangeSummation 7 == 84 (that is 7+8+9+10+11+12+13+14)

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
-- myconcat [[1,2,3,4,5], [6,7,8]] == [1,2,3,4,5,6,7,8]
-- myconcat [[6,7,8], [1,2,3,4,5]] == [6,7,8,1,2,3,4,5]

-- Exercise 17
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x < y then x:y:ys else y : insert x ys
-- insert 7 [5,6,8,9] == [5,6,7,8,9]
-- insert 1 [2,3,4,5] == [1,2,3,4,5]
insertionsort :: Ord a => [a] -> [a]
insertionsort [x] = [x]
insertionsort (x:xs) = insert x (insertionsort xs)
-- insertionsort [8,9,4,3,1,6] == [1,3,4,6,8,9]
-- insertionsort [1,10,34,2,1] == [1,1,2,10,34]

-- Exercise 18
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = quicksort (filter (\x -> x <= p) xs) ++ [p] ++ (quicksort (filter (\x -> x > p) xs) )
-- quicksort [8,9,4,3,1,6] == [1,3,4,6,8,9]
-- quicksort [1,10,34,2,1] == [1,1,2,10,34]

-- Exercise 19
evensB :: [Integer] -> [Integer]
evensB ys  = [x | x <- ys, even x]
-- evensB [1,2,3,4,5,6] == [2,4,6]
-- evensB [1,1,3,4,5,6] == [4,6]

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
mylast, mylast' :: [a] -> a
mylast [] = error "no last element in empty list"
mylast xs = if length xs == 1 then head xs else mylast (tail xs)
mylast' [] = error "no last element in empty list"
mylast' (x:[]) = x
mylast' (x:xs) = mylast' xs
-- mylast [1,2,3,4] == 4
-- mylast [1,2,3,4,9] == 9
-- mylast' [1,2,3,4] == 4
-- mylast' [1,2,3,4,9] == 9

-- Exercise 24
mylastb :: [a] -> a
mylastb [] = error "no last element on empty list"
mylastb xs = if length xs == 1 then head xs else mylastb (drop (length xs -1) xs)
-- mylastb [1,2,3,4] == 4
-- mylastb [1,2,3,4,9] == 9

-- Exercise 25
myinit, myinitb, myinitc :: [a] -> [a]
myinit [] = error "no init part in empty list"
myinit (x:xs) = if length xs == 0 then [] else  x:myinit xs 
myinitb [] = error "no init part in empty list"
myinitb xs = take (length xs - 1) xs
myinitc [] = error "no init part in empty list" 
myinitc (x:[]) = []
myinitc (x:xs) = x:myinitc xs
-- myinit [1,2,3,4] == [1,2,3]
-- myinit [1,2,3,4,9] == [1,2,3,4]
-- myinitb [1,2,3,4] == [1,2,3]
-- myinitb [1,2,3,4,9] == [1,2,3,4]
-- myinitc [1,2,3,4] == [1,2,3]
-- myinitc [1,2,3,4,9] == [1,2,3,4]

-- Exercise 26
mysecondconcat :: [[a]] -> [a]
mysecondconcat xs = foldr (++) [] xs
-- mysecondconcat [[1,2,3],[4,5,6]] == [1,2,3,4,5,6]
-- mysecondconcat [[4,5,6], [1,2,3]] == [4,5,6,1,2,3]

mysecondreverse :: [a] -> [a]
mysecondreverse xs = foldr (\ x acc -> acc ++ [x]) [] xs
-- mysecondreverse [1,2,3,4,5,6] == [6,5,4,3,2,1]
-- mysecondreverse [6,5,4,3,2,1] == [1,2,3,4,5,6]

-- Exercise 27
prefix :: [a] -> [[a]]
prefix xs = mysecondreverse (prefix' xs)
prefix' :: [a] -> [[a]]
prefix' [] = [[]]
prefix' xs = xs : (prefix' (init xs))
-- prefix [1,2,3] ==  [[],[1],[1,2],[1,2,3]]
-- prefix [3,2,1,0] == [[],[3],[3,2],[3,2,1],[3,2,1,0]]
