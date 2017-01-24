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
isredex (App t1 S) = True
isredex (App t1 K) = True
isredex (App t1 I) = True
isredex (App t1 t2) = False

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
outerstep (App t1 t2) =
  if isredex (App t1 t2)
  then dostep (findstep (App t1 t2)) (App t1 t2) 
  else (App t1 t2)
  
-- finds the Cl-term to apply the rule from
findstep (App t1 t2) =
  if isnormalform (App t1 t2)
  then t1
  else findstep t1

-- dostep performs one step according to the Cl-terms rules
dostep I (App t1 t2) = resolveI (App t1 t2) 
dostep K (App t1 t2) = resolveK (App t1 t2)
dostep S (App t1 t2) = resolveS (App t1 t2)
resolveI (App I t2) = t2
resolveI (App t1 t2) = (App (resolveI t1) t2)
resolveK (App K t2) = t2
resolveK (App t1 t2) =
  if isnormalform t1
  then (resolveK t1)
  else (App (resolveK t1) t2)
resolveS (App t1 t2) =
  if isnormalform (returnT1(t1))
  then App (App (returnT2(returnT1(t1))) t2) (App (returnT2(t1)) t2)
  else (App (resolveS t1) t2)

-- returns the first term
returnT1 (App t1 t2) = t1
-- returns the second term
returnT2 (App t1 t2) = t2
  
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

