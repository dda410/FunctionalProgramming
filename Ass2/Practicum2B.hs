module Practicum2B where

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

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)


-- given: definition of the monad MaybeOne used for partial functions
data MaybeOne a = NoResult
                | Result a deriving (Show, Eq)

instance Applicative MaybeOne where
    pure x = Result x
    (<*>)  = ap

instance Functor MaybeOne where
    fmap = liftM

instance Monad MaybeOne where
  NoResult   >>= _ = NoResult
  (Result n) >>= f = f n
  return x         = Result x


-- Example: partial function for division
myDividedBy :: Double -> Double -> MaybeOne Double
myDividedBy n d =
  if d  == 0
  then NoResult
  else Result (n / d)

-- Exercise 1-a
myIndexOf :: [Double] -> Double -> MaybeOne Int
myIndexOf l n =
  if (myIndexOf' l n) > (length l)
  then NoResult
  else Result (myIndexOf' l n)

myIndexOf' [] n = 1
myIndexOf' (h:l) n =
  if h == n
  then 1
  else 1 + (myIndexOf' l n)
-- myIndexOf [1,2,3] 2 == Result 2
-- myIndexOf [1,2,3] 5 == NoResult

-- Exercise 1-b
myRemainderString :: String -> String -> MaybeOne String
myRemainderString x y =
  if take (length x) y == x
  then Result (drop (length x) y)
  else NoResult

-- Create an operator for our divide function
n // d = n `myDividedBy` d

-- Example f using case (1/2)
f :: Double -> Double -> Double -> MaybeOne Double
f x y z = case x // y of
    NoResult           -> NoResult
    Result xDividedByy ->
      case xDividedByy // z of
        NoResult   -> NoResult
        Result   r -> Result r

-- Example f using case (2/2)
fShorter :: Double -> Double -> Double -> MaybeOne Double
fShorter x y z = case x // y of
    NoResult           -> NoResult
    Result xDividedByy -> xDividedByy // z

-- Example g using case
g :: Double -> Double -> Double -> Double -> MaybeOne Double
g x y z s =
  case x // y of
    NoResult           -> NoResult
    Result xDividedByy ->
      case y // z of
        NoResult           -> NoResult
        Result xDividedByz ->
          case y // s of
            NoResult           -> NoResult
            Result yDividedBys ->
              case z // s of
                NoResult           -> NoResult
                Result zDividedBys ->
                  let n = yDividedBys + zDividedBys
                      d = xDividedByy - xDividedByz
                  in n // d


-- Exercise 2
v1 :: Double -> Double -> Double -> Double -> MaybeOne Double
v1 x y z s =
  case x // y of
    NoResult -> NoResult
    Result xDBy ->
      case z // s of
        NoResult -> NoResult
        Result zDBs ->
          case y // s of
            NoResult -> NoResult
            Result yDBs ->
              case xDBy // (zDBs - yDBs) of
                NoResult -> NoResult
                Result left ->
                  case z // x of
                    NoResult -> NoResult
                    Result zDBx ->
                      let right = yDBs + zDBx
                          in Result (left - right)

-- Example f using >==
fBetter :: Double -> Double -> Double -> MaybeOne Double
fBetter x y z = (x // y) >>= dividedByZ
  where dividedByZ xdividedByy = xdividedByy // z

-- Example f using >= and lambda 
fBetterLambda :: Double -> Double -> Double -> MaybeOne Double
fBetterLambda x y z = (x // y) >>= (\xDividedByy -> xDividedByy // z)

-- Example g using >== and lambda
gBetter :: Double -> Double -> Double -> Double -> MaybeOne Double
gBetter x y z s =
  (x // y) >>=
  (\xDividedByy ->
    (x // z) >>=
    (\xDividedByz ->
      let d = xDividedByy - xDividedByz
      in (y // s) >>=
      (\yDividedBys ->
        (z // s) >>=
        (\zDividedBys ->
          let n = yDividedBys + zDividedBys
          in n // d
        )
      )
    )
  )

-- Exercise 3
v2 :: Double -> Double -> Double -> Double -> MaybeOne Double
v2 x y z s =
  (x // y) >>=
  (\xDBy ->
     (z // s) >>=
     (\zDBs ->
        (y // s) >>=
        (\yDBs ->
           (xDBy // (zDBs - yDBs)) >>=
           (\left ->
              (z // x) >>=
              (\zDBx ->
                 let right = yDBs + zDBx
                 in Result (left - right)
              )
           )
        )
     )
  )


-- Example f using do
fDo :: Double -> Double -> Double -> MaybeOne Double
fDo x y z = do
  xDividedByy <- x // y
  xDividedByy // z

-- Example g using do
gDo :: Double -> Double -> Double -> Double -> MaybeOne Double
gDo x y z s = do
  xDividedByy <- x // y
  xDividedByz <- y // z
  let d = xDividedByy - xDividedByz
  yDividedBys <- y // s
  zDividedBys <- z // s
  let n = yDividedBys + zDividedBys
  n // d

-- Example f using do-return
fPerfect :: Double -> Double -> Double -> MaybeOne Double
fPerfect x y z = do
  xDividedByy <- x // y
  result      <- xDividedByy // z
  return result

-- Example g using do-return
gPerfect :: Double -> Double -> Double -> Double -> MaybeOne Double
gPerfect x y z s = do
  xDividedByy <- x // y
  xDividedByz <- y // z
  let denominator = xDividedByy - xDividedByz
  yDividedBys <- y // s
  zDividedBys <- z // s
  let numerator = yDividedBys + zDividedBys
  result <- numerator // denominator
  return result

-- Exercise 4
v3 :: Double -> Double -> Double -> Double -> MaybeOne Double
v3 x y z s = undefined

