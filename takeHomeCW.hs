Бисярина Марианна, группа 1539
takeHomeKP


1) Приведите в форму не использующую ассоциативности операторов и
синтаксического сахара ("сахар развернуть, расставить все скобки"):

* λ x y → y x x
\x -> (\y -> ((y x) x))

* λ _ y → y (λ a _ → a)
\x -> (\y -> (y (\a -> (\b -> a))))

* λ f g x y → f (g x) y
\f -> (\g -> (\x -> (\y -> ((f (g x)) y))))

* λ f g h x y → f (g x) h y
\f -> (\g -> (\h -> (\x -> (\y -> (((f (g x)) h) y)))))

* λ f g h x y → f (g x) (h y)
\f -> (\g -> (\h -> (\x -> (\y -> ((f (g x)) (h y))))))

* :: a → b → c
    a -> (b -> c)
* :: a → b → c → d
    a -> (b -> (c -> d))
* :: (a → b → c) → d
    (a -> (b -> c)) -> d



1) Приведите в форму, использующую все возможные ассоциативности
операторов и синтаксический сахар ("свернуть в сахар, убрать все
лишние скобки"):

* λ x y → x
\x _ -> x

* λ x → λ y → y
\_ y -> y

* λ x y → (λ y → ((x (λ a b → y)) y))
\ x _ y -> x (\_ _ -> y) y

* (λ x → x) y
y

* :: a → (b → c)
    :: a -> b -> c
* :: (a → b) → c
    :: (a -> b) -> c

* :: (a → (b → c)) → c    
    :: (a -> b -> c) -> c




2) Привести в нормальную форму ("некуда дальше упрощать"):

* y (λ x → x)
    y (\x -> x)

* (λ x → x) y
    y

* λ x y → x
\x _ -> x

* λ x y → (λ x -> x) y
\_ y -> y

* λ x y → y (λ x -> x)
\_ y -> y (\x -> x)

* λ x → y (λ x -> x) y
\_ -> y (\x -> x) y

* λ f g → (. f) . g
\f g x y -> g x (f y)

* λ f g → map (f . g), стандартный map :: (a → b) → [a] → [b]

\f g l -> case l of
                 []     -> []
                 (x:xs) -> f (g x) : case xs of
                                             []     -> []
                                             (y:ys) -> f (g y) : ... 
-- т.д. разворачивать


* filter (λ x → True), стандартный filter :: (a → Bool) → [a] → [a]

filter (\x -> True) l = case l of 
                             [] -> []
                             (x:xs) = ((\x -> True) l) : (filter (\x -> True) xs) -- и т.д. разворачивать     


(В последних двух надо упрощать внутри case.)
















3) Напишите систему уравнений и результирующий тип для

* λ f g x → f (g x)
x :: a
g :: a -> b
f :: b -> c
func :: (b -> c) -> (a -> b) -> a -> c


* λ f g h x → f (g (h x))
x :: a
h :: a -> b
g :: b -> c
f :: c -> d

func :: (c -> d) -> (b -> c) -> (a -> b) -> a -> d

* λ f g h x y → f (g x) (h y)

x :: a
y :: b
g :: a -> c
h :: b -> d
f :: c -> d -> e

func :: (c -> d -> e) -> (a -> c) -> (b -> d) -> a -> b -> e

* λ f g x → if x == 0 then f g else g

== :: a -> a -> Bool
0 :: Integer
x :: Integer
if _ then _ else _ ::  Bool -> b 
g :: b
f g :: b
f :: b -> b

func :: (b -> b) -> b -> Integer -> b




















4) Напишите операции вставки и удаления для следующего определения
бинарного дерева поиска:

{-# LANGUAGE UnicodeSyntax #-}
data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving Show

insert :: Ord a => a → Tree a → Tree a
insert newVer Leaf = Node newVer Leaf Leaf
insert newVer (Node x l r)
    | newVer < x = Node x (insert newVer l) r
    | newVer > x = Node x l (insert newVer r)
    | newVer == x = Node x l r

delete :: Ord a => a → Tree a → Tree a
delete ver Leaf = Leaf

delete ver (Node x Leaf r) 
    | ver < x = Node x Leaf r
    | ver > x = Node x Leaf (delete ver r)
    | ver == x = r
            
delete ver (Node x l Leaf) 
    | ver < x = Node x (delete ver l) Leaf
    | ver > x = Node x l Leaf
    | ver == x = l

delete ver (Node x l r) 
    | ver < x = Node x (delete ver l) r
    | ver > x = Node x l (delete ver r)
    | ver == x = Node m l (delete m r) 
            where m = findMin r
 
--function finding min vertex in tree
findMin :: Ord a => Tree a -> a
findMin (Node x Leaf _) = x
findMin (Node _ l _) = findMin l





















5) Напишите классы Monoid, Group, Ring, Field. Запишите ньютоновские
уравнения для движения без трения с их помощью.

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

































А теперь физика

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Physics where

import Base
import Algebra


-- Движение по прямой согласно законам Ньютона

type Mass = Double
type Coordinate = Double
type Velocity = Double
type Acceleration = Double
type Time = Double
type Force = Double

data Body = Body { m :: Mass, 
                   x :: Coordinate, 
                   v :: Velocity,
                   a :: Acceleration } 

-- Координата, в которой окажется тело b через время t
destination :: Body -> Time -> Coordinate
destination b t = (x b) + t * (v b) + t * t * (a b) * (inverted 2)

-- Скорость тела  b через время t
velocity :: Body -> Time -> Velocity
velocity b t = (v b) + (a b) * t

-- Ускорение тела b под действием силы f

acceleration :: Body -> Acceleration
acceleration b = a b 


-- Координата, в которой окажется тело b через время t под действием силы f
destinationWithForce :: Body -> Force -> Time -> Coordinate
destinationWithForce b f t = (x b) + t * (v b) + (a b + (getA f (m b))) * t * t * (inverted 2)

-- Скорость тела  b через время t под действием силы f
velocityWithForce :: Body -> Force -> Time -> Velocity
velocityWithForce b f t = (v b) + (a b + (getA f (m b))) * t

-- Ускорение тела b под действием силы f

accelerationWithForce :: Body -> Force -> Acceleration
accelerationWithForce b f = (a b + (getA f (m b)))


-- Увеличение ускорения тела с массой m под действием силы f
getA :: Force -> Mass -> Acceleration
getA f m = f * (inverted m)


--Тело b через время t
move :: Body -> Time -> Body
move b t = b { x = destination b t }

-- Тело b через время t при действии на него силы f
moveWithForce :: Body -> Force -> Time -> Body
moveWithForce b f t = b { x = destinationWithForce b f t, v = velocityWithForce b f t, a = accelerationWithForce b f }
