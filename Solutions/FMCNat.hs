{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    , fst
    , snd
    )


-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO

    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where

    -- Para o O, sempre True:
    
    (==) :: Nat -> Nat -> Bool
    O == O = True

    -- Para sucessores, usar recursão
    S n == S m = n == m

    -- Else: False?
    _ == _ = False

instance Ord Nat where

    O <= _ = True
    S _ <= O = False
    S n <= S m = n <= m
    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    -- Definição Recursiva para Sucessores

    
    min :: Nat -> Nat -> Nat
    min O _ = O
    min _ O = O
    -- Duas para garantir a identidade

    min (S n) (S m) = S (min n m)

    max :: Nat -> Nat -> Nat
    max O m = m
    max m O = m
    max (S n) (S m) = S (max n m)




----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero (S n) = False 

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n  

even :: Nat -> Bool
-- Output: O means False, S O means True
even O = True -- True
even (S O) = False -- Even do sucessor de 0 é false
even (S (S n)) = even n -- "Decompõe" em dois para responder true

odd :: Nat -> Bool
odd O = False
odd (S O) = True
odd (S (S n)) = odd n


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O = n
n <+> S m = S(n <+> m)
infixl 6 <+>


-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus O _ = O
monus n O = n
monus (S n) (S m) = monus n m

infixl 6 <->

(<->) :: Nat -> Nat -> Nat
(<->) = monus

-- multiplication
times :: Nat -> Nat -> Nat
times _ O = O
times O _  = O
times n (S p) = n <*> p <+> n

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

infixl 7 <*>

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow = (<^>)

exp :: Nat -> Nat -> Nat
exp = (<^>)

(<^>) :: Nat -> Nat -> Nat
_ <^> O   = S O
n <^> S m = n <^> m <*> n

infix 8 <^>

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) = undefined

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) = undefined

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (_, O) = undefined
eucdiv (n, m)
  | n < m = (O, n)
  | otherwise = (q, r)
  where
    q = S q'
    r = r'
    (q', r') = eucdiv (n <-> m, m)

-- divides
divides :: Nat -> Nat -> Bool
divides = (<|>)

(<|>) :: Nat -> Nat -> Bool
O <|> n = isZero n
n <|> m = isZero (m <%> n)


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
(|-|) :: Nat -> Nat -> Nat
n |-| m =
  if n < m
    then m <-> n
    else n <-> m

infixl 6 |-|

factorial :: Nat -> Nat
factorial O = S O -- Essa definimos na aula
factorial (S n) = S n * factorial n --- Define recursivamente

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O 
sg _ = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo _ O = undefined 
lo O _ = undefined
lo n m =
  case m </> n of
    O -> O
    _ -> S (lo n (m </> n))


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: (Integral a) => a -> Nat
toNat x
  | x < 0     = undefined
  | x == 0    = O
  | otherwise = S (toNat (x - 1))

fromNat :: (Integral a) => Nat -> a
fromNat O     = 0
fromNat (S n) = 1 + fromNat n

-- Voilà: we can now easily make Nat an instance of Num.
instance Num Nat where
  (+) = (<+>)
  (*) = (<*>)
  (-) = (<->)
  abs n = n
  signum = sg
  fromInteger x
    | x < 0 = undefined
    | x == 0 = O
    | otherwise = S (fromInteger (x - 1))