{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Algebra where

import Base

class SemiGroup a where
-- forall a b c. (a <> b) <> c == a <> (b <> c)
    (<>) :: a -> a -> a

class SemiGroup a => Monoid a where
-- forall a. a <> neutral == neutral <> a == a
     neutral :: a
     
class Monoid a => Group a where 
-- forall a. a <> (inverse a) == (inverse a) <> a == neutral
    inverse :: a -> a

class Group a => AbelianGroup a 
-- forall a b. a <> b == b <> a



newtype Add a = Add { sumVal :: a }
newtype Mul a = Mul { mulVal :: a }

instance SemiGroup (Add Double) where
    (Add x) <> (Add y) = Add (sumDouble x y)

instance Monoid (Add Double) where
    neutral = Add 0

instance SemiGroup (Mul Double) where
    (Mul x) <> (Mul y) = Mul (mulDouble x y)


instance Monoid (Mul Double) where
    neutral = Mul 1


instance Group (Add Double) where
    inverse (Add x) = Add (-x)

instance AbelianGroup (Add Double)


class (AbelianGroup (Add a)) => AddGroup a where  
    infixl 4 +
    (+) :: a -> a -> a
    zero :: a
    negative :: a -> a

    x + y = sumVal ((Add x) <> (Add y))
    zero = sumVal neutral
    negative x = sumVal (neutral <> (inverse (Add x)))


class (Monoid (Mul a)) => MulGroup a where  
    infixl 6 *
    (*) :: a -> a -> a
    one :: a
    
    x * y = mulVal ((Mul x) <> (Mul y))
    one = mulVal neutral

instance AddGroup Double
instance MulGroup Double

class (AddGroup a, MulGroup a) => Ring a where
--forall a b c. (a + b) * c == a * c + b * c
--forall a b c. a * (b + c) == a * b + a * c

class Ring a => Field a where 
--forall a except zero. a * (inverted a) == one
    inverted :: a -> a

instance Ring Double
instance Field Double where
    inverted = divDouble 1.0
