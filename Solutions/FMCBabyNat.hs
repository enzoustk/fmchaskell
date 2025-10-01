module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined, Num (negate) )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- some sugar
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

-- addition
(+) :: Nat -> Nat -> Nat
n + O   = n
n + S m = S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero O = S O
isZero (S n) = O 

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n  

-- Output: O means False, S O means True
even :: Nat -> Nat
even O = S O -- True
even (S O) = O -- Even do sucessor de 0 é false
even (S (S n)) = even n -- "Decompõe" em dois para responder true

odd :: Nat -> Nat -- Inverso do Even?
odd O = O
odd (S O) = S O
odd (S (S n)) = odd n

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus O _ = O
monus n O = n
monus (S n) (S m) = monus n m

(-*) :: Nat -> Nat -> Nat
(-*) = monus

-- multiplication
(*) :: Nat -> Nat -> Nat
_ * O = O
O * _ = O
n * (S p) = n * p + n

infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
_ ^ O = S O
n ^ (S m) = n ^ m * n -- só adiciona mais um produto

infixr 8 ^

-- menor ou igual: para definir o quociente, posteriormente
(<=) :: Nat -> Nat -> Nat
O <= n = S O -- True
S n <= O = O -- False
(S n) <= (S m) = n <= m

infix 4 <=

-- quotient
(/) :: Nat -> Nat -> Nat -- Vai vendo quantas vezes um cabe no outro até "topar"
n / O = undefined -- Indeterminação?
n / m = -- por casos:
  case m <= n of
    O -> O -- Caso que case é false (Não divide -> retorna O)
    S O -> S ((n -* m) / m) -- Caso que case é True -> divide

-- remainder
(%) :: Nat -> Nat -> Nat -- Já tenho o valor que divide, agora preciso do que sobra
n % O = undefined -- mesmo princípio
n % m =
  case m <= n of
    O   -> n                  -- se m > n, sobra tudo
    S O -> (n -* m) % m       -- senão, tira m e continua

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
O ||| n = undefined -- o 0 divide N? -Não existe um k tal que 0k =n
m ||| n = 
  case n % m of -- ver se há resto
    O -> S O -- Se não tem, divide
    _ -> O

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat -- Diferença absoluta: se tiver ao contrário, eu inverto?
absDiff n m =
  case n -* m of -- ver se estão na posição certa
    O -> m -* n -- Se não estiver, troca
    _ -> n -* m -- se estiver, só resolve

(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff

factorial :: Nat -> Nat
factorial O = S O -- Essa definimos na aula
factorial (S n) = S n * factorial n --- Define recursivamente

-- signum of a number (-1, 0, or 1)
-- Só temos positivos?
sg :: Nat -> Nat
sg O = O 
sg _ = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo _ O = undefined 
lo O _ = undefined
lo n m =
  case m / n of
    O -> O
    _ -> S (lo n (m / n))

