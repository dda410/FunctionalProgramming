module Practicum3 where

{-
Name:           <Dimitri Diomaiuta>
VU-net id:      <dda410>
Student number: <2553339>
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
isredex (App I t2) = True
isredex (App (App K t2) t3) = True
isredex (App (App (App S t2) t3) t4) = True
isredex t = False

isnormalform :: Term -> Bool
isnormalform I = True
isnormalform K = True
isnormalform S = True
isnormalform (App t1 t2) = isnormalform' t1 && isnormalform' t2
isnormalform' I = True
isnormalform' K = True
isnormalform' S = True
isnormalform' (App t1 t2) = False

outerstep :: Term -> Term
outerstep (App I t2) = t2
outerstep (App (App K t2) t3) = t2
outerstep (App (App (App S t2) t3) t4) = (App (App t2 t4) (App t3 t4))
outerstep t = t

-- Exercise  3
data Thing = C | D | B
  deriving (Show, Eq, Bounded, Enum)

nxt :: Thing -> Thing
nxt C = B
nxt B = D
nxt D = C

-- Exercise 4
data I = Z | O
  deriving (Show, Eq, Bounded, Enum)

s :: I -> I
s Z = O
s O = Z

p :: I -> I
p Z = O
p O = Z

