module Practicum3 where

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

-- Exercise 1
data IntExp  = Lit Int | Add IntExp IntExp | Mul IntExp IntExp
  deriving Show

showintexp :: IntExp -> String
showintexp (Lit x) = show x
showintexp (Add (Lit x) (Lit y)) = "(" ++ show x ++ "+" ++ show y ++ ")"
showintexp (Mul (Lit x) (Lit y)) = "(" ++ show x ++ "*" ++ show y ++ ")"

evalintexp :: IntExp -> Int
evalintexp (Lit x) = x
evalintexp (Add (Lit x) (Lit y)) = x+y
evalintexp (Mul (Lit x) (Lit y)) = x*y

-- Exercise 2
data Term = S | K | I | App Term Term

instance Show Term where
  show a = showterm a

showterm :: Term -> String
showterm S = "S"
showterm K = "K"
showterm I = "I"
showterm (App t1 t2) = "(" ++ (showterm t1) ++ (showterm t2) ++ ")"

isredex :: Term -> Bool
isredex = undefined

isnormalform :: Term -> Bool
isnormalform = undefined

outerstep :: Term -> Term
outerstep = undefined

-- Exercise  3
data Thing = Undefined1
  deriving (Show, Eq, Bounded, Enum)

nxt :: Thing -> Thing
nxt = undefined

-- Exercise 4
data I = Undefined2
  deriving (Show, Eq, Bounded, Enum)

s :: I -> I
s = undefined

p :: I -> I
p = undefined

