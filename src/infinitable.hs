module Infinitable where

import Data.Ord

data Infinitable a = NegInfty
                   | Finite a
                   | Infinity
                   | Indeterminate
                   deriving ( Eq )

instance Bounded (Infinitable a) where
   minBound = NegInfty
   maxBound = Infinity

mFromEnum :: (Enum a) => Infinitable a -> Maybe Int
mFromEnum (Finite x) = Just $ fromEnum x
mFromEnum _          = Nothing

instance (Enum a, Ord a) => Enum (Infinitable a) where
   
   -- Makes an integer an Infinitable number
   toEnum = Finite . toEnum

   -- Any type of infinity can't be represented as an integer
   fromEnum (Finite a)    = fromEnum a
   fromEnum Infinity      = error "fromEnum: can't represent infinity as a (finite) integer"
   fromEnum NegInfty      = error "fromEnum: can't represent negative infinity as a (finite) integer"
   fromEnum Indeterminate = error "fromEnum: can't represent an indeterminate value as an integer"

   -- Successors
   succ (Finite n) = Finite (succ n)
   succ infinity   = infinity

   -- Predecessors
   pred (Finite n) = Finite (pred n)
   pred infinity   = infinity

   -- Sequences

   enumFromThen x y   = enumFromThenTo x y        Infinity
   enumFromTo   x   z = enumFromThenTo x (succ x) Infinity

   enumFromThenTo Indeterminate _ _ = [Indeterminate]
   enumFromThenTo x Indeterminate _ = [x,Indeterminate]
   enumFromThenTo x y Indeterminate = enumFromThen x y

   enumFromThenTo (Finite x) (Finite y) (Finite z) = map Finite [x, y .. z]

   enumFromThenTo (Finite x) (Finite y) Infinity   | y >= x    = map Finite [x, y ..]
                                                   | otherwise = []
   enumFromThenTo (Finite x) (Finite y) NegInfty   | y <= x    = map Finite [x, y ..]
                                                   | otherwise = []

   enumFromThenTo (Finite x) Infinity   (Finite z) | x <= z    = [Finite x]
                                                   | otherwise = []
   enumFromThenTo (Finite x) NegInfty   (Finite z) | x >= z    = [Finite x]
                                                   | otherwise = []

   enumFromThenTo (Finite x) Infinity   Infinity   = [Finite x,Infinity]
   enumFromThenTo (Finite _) Infinity   NegInfty   = []
   enumFromThenTo (Finite _) NegInfty   Infinity   = []
   enumFromThenTo (Finite x) NegInfty   NegInfty   = [Finite x,NegInfty]

   enumFromThenTo Infinity   (Finite y) (Finite z) | y >= z =    [Infinity,Finite y]
                                                   | otherwise = [Infinity]
   enumFromThenTo NegInfty   (Finite y) (Finite z) | y <= z =    [NegInfty,Finite y]
                                                   | otherwise = [NegInfty]

   enumFromThenTo Infinity   (Finite _) Infinity   = [Infinity]
   enumFromThenTo Infinity   (Finite y) NegInfty   = [Infinity,Finite y,NegInfty]
   enumFromThenTo NegInfty   (Finite y) Infinity   = [NegInfty,Finite y,Infinity]
   enumFromThenTo NegInfty   (Finite _) NegInfty   = [NegInfty]

   enumFromThenTo Infinity   _          Infinity   = [Infinity]
   enumFromThenTo NegInfty   _          NegInfty   = [NegInfty]
   enumFromThenTo Infinity   _          NegInfty   = repeat Infinity
   enumFromThenTo NegInfty   _          Infinity   = repeat NegInfty

mCompare :: (Ord a) => Infinitable a -> Infinitable a -> Maybe Ordering
mCompare Indeterminate Indeterminate = Just EQ
mCompare Indeterminate _             = Nothing
mCompare _             Indeterminate = Nothing
mCompare x             y             = Just $ compare x y

instance (Ord a) => Ord (Infinitable a) where
   compare Indeterminate Indeterminate = EQ
   compare Infinity      Infinity      = EQ
   compare NegInfty      NegInfty      = EQ
   compare Indeterminate _ = error "compare: can't compare an indeterminate value with a non-indeterminate value"
   compare _ Indeterminate = error "compare: can't compare an indeterminate value with a non-indeterminate value"
   compare NegInfty Infinity = LT
   compare Infinity NegInfty = GT
   compare NegInfty (Finite n) = LT
   compare Infinity (Finite n) = GT
   compare (Finite n) Infinity = LT
   compare (Finite n) NegInfty = GT
   compare (Finite a) (Finite b) = compare a b

instance (Show a) => Show (Infinitable a) where
   show NegInfty      = "-Infinity"
   show Infinity      = "Infinity"
   show Indeterminate = "Indeterminate"
   show (Finite n)    = "Finite " ++ show n

instance (Eq a, Num a) => Num (Infinitable a) where

   -- Addition...
   -- of finite numbers:
   Finite a + Finite b = Finite (a + b)
   -- of infinite numbers:
   NegInfty + NegInfty = NegInfty
   NegInfty + Infinity = Indeterminate
   Infinity + Infinity = Infinity
   -- of mixed numbers:
   Infinity + Finite n = Infinity
   NegInfty + Finite n = NegInfty
   -- for indeterminates
   Indeterminate + _ = Indeterminate
   -- switch operands:
   a + b = b + a

   -- Multiplication...
   -- of finite numbers:
   Finite a * Finite b = Finite (a * b)
   -- of infinite numbers:
   NegInfty * NegInfty = Infinity
   NegInfty * Infinity = NegInfty
   Infinity * Infinity = Infinity
   -- of mixed numbers:
   _        * Finite 0 = Indeterminate
   Infinity * Finite n | signum n == 1 = Infinity
                       | otherwise     = NegInfty
   NegInfty * Finite n = Infinity * Finite (negate n)
   -- for indeterminates:
   Indeterminate * _ = Indeterminate
   -- switch operands:
   a * b = b * a

   -- Negation...
   negate (Finite n)    = Finite (negate n)
   negate NegInfty      = Infinity
   negate Infinity      = NegInfty
   negate Indeterminate = Indeterminate

   -- Absolute value...
   abs (Finite n)    = Finite (abs n)
   abs Infinity      = Infinity
   abs NegInfty      = Infinity
   abs Indeterminate = Indeterminate

   -- Signum...
   signum (Finite n)    = Finite (signum n)
   signum NegInfty      = -1
   signum Infinity      =  1
   signum Indeterminate = Indeterminate

   -- From integer...
   fromInteger = Finite . fromInteger

mToRational :: (Real a) => Infinitable a -> Maybe Rational
mToRational (Finite x) = Just $ toRational x
mToRational _          = Nothing

instance (Real a) => Real (Infinitable a) where
   toRational (Finite x)    = toRational x
   toRational Infinity      = error "toRational: can't represent infinity as a (finite) rational"
   toRational NegInfty      = error "toRational: can't represent negative infinity as a (finite) rational"
   toRational Indeterminate = error "toRational: can't represent an indeterminate value as an rational"

mToInteger :: (Integral a) => Infinitable a -> Maybe Integer
mToInteger (Finite x) = Just $ toInteger x
mToInteger _          = Nothing

instance (Eq a, Integral a) => Integral (Infinitable a) where
   toInteger (Finite x)    = toInteger x
   toInteger Infinity      = error "toInteger: can't represent infinity as a (finite) integer"
   toInteger NegInfty      = error "toInteger: can't represent negative infinity as a (finite) integer"
   toInteger Indeterminate = error "toInteger: can't represent an indeterminate value as an integer"

   quotRem _    (Finite 0) = (Indeterminate,Indeterminate)
   quotRem _ Indeterminate = (Indeterminate,Indeterminate)
   quotRem Indeterminate _ = (Indeterminate,Indeterminate)
   quotRem (Finite x) (Finite y) = (Finite (quot x y), Finite (rem x y))
   quotRem (Finite _) Infinity   = (0,Indeterminate)
   quotRem (Finite _) NegInfty   = (0,Indeterminate)
   quotRem Infinity (Finite n) | signum n == 1 = (Infinity,Indeterminate)
                               | otherwise     = (NegInfty,Indeterminate)
   quotRem NegInfty (Finite n) | signum n == 1 = (NegInfty,Indeterminate)
                               | otherwise     = (Infinity,Indeterminate)
   quotRem _ _ = (Indeterminate,Indeterminate)

instance (Eq a, Fractional a) => Fractional (Infinitable a) where

   _        / Finite 0      = Indeterminate
   _        / Indeterminate = Indeterminate
   Indeterminate / _        = Indeterminate
   Finite a / Finite b = Finite (a / b)
   Finite _ / Infinity = 0
   Finite _ / NegInfty = 0
   Infinity / (Finite n) | signum n == 1 = Infinity
                         | otherwise     = NegInfty
   NegInfty / (Finite n) | signum n == 1 = NegInfty
                         | otherwise     = Infinity
   _ / _ = Indeterminate

   -- Reciprocals...
   recip (Finite 0)    = Indeterminate
   recip (Finite n)    = Finite (recip n)
   recip Indeterminate = Indeterminate
   recip _             = 0

   -- From rationals...
   fromRational = Finite . fromRational

eitherFromInf :: Infinitable a -> Either (Infinitable a) a
eitherFromInf (Finite number) = Right number
eitherFromInf infinity        = Left  infinity
