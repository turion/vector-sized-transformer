

data Nat where
  Z :: Nat
  S :: Nat -> Nat

addPeano :: Nat -> Nat -> Nat
addPeano Z n = n
addPeano (S n) m = S $ addPeano n m

data Vector (n :: Nat) a where
  VNil :: Vector 'Z a
  (:::) :: a -> Vector n a -> Vector ('S n) a

head :: Vector ('S n) a -> a
head (a ::: _) = a

instance Functor (Vector n) where
  fmap _ VNil = VNil
  fmap f (a ::: as) = f a ::: fmap f as

type Matrix n m a = Vector n (Vector m a)


newtype ListT m a = ListT (m (ListT' m a))
data ListT' m a = TNil | a :+: ListT m a

-- data [a] = Nil | Cons a | Cons2 a a | Cons3 a a a | ...
-- data ListT m a != Nil (m ()) | Cons (m (a, m ())) | Cons2 (m (a, m (a, m ()))) | ...

-- type family 

-- append :: Vector n a -> Vector m a -> Vector (addPeano n m) a
-- append VNil v = v
