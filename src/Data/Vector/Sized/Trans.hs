{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Data.Vector.Sized.Trans where

-- base
import Control.Arrow (second)
import GHC.TypeNats
import Prelude hiding (replicate, foldr, mapM, traverse)
import Data.Functor.Identity
import Data.Functor.Const
import Data.Proxy
import Unsafe.Coerce
import Control.Monad hiding (replicateM)

-- transformers
import Control.Monad.Trans.Class

-- mmorph
import Control.Monad.Morph (MFunctor (..))

data VectorT (n :: Nat) m a where
  VNil :: VectorT 0 m a
  VCons :: m (a, VectorT n m a) -> VectorT (n + 1) m a

infixr `consM`

consM :: Functor m => m a -> VectorT n m a -> VectorT (n + 1) m a
consM ma mas = VCons $ ( , mas) <$> ma

consM' :: Functor m => a -> m (VectorT n m a) -> VectorT (n + 1) m a
consM' a mas = VCons $ (a, ) <$> mas

-- FIXME look at all the classes vector-sized has and get them as well

deriving instance ((forall x . Show x => Show (m x)), Show a) => Show (VectorT n m a)

deriving instance Functor m => Functor (VectorT n m)

deriving instance Foldable m => Foldable (VectorT n m)
deriving instance (Functor m, Foldable m, Traversable m) => Traversable (VectorT n m)

instance (KnownNat n, Applicative m) => Applicative (VectorT n m) where
  pure = replicate

  VNil <*> VNil = VNil
  VCons vf <*> VCons va = VCons $ (\(f, fs) (a, as) -> (f a, fs <*> as)) <$> vf <*> va

instance MFunctor (VectorT n) where
  hoist _ VNil = VNil
  hoist morph (VCons v) = VCons $ morph $ second (hoist morph) <$> v

singleton :: Functor m => m a -> VectorT 1 m a
singleton = VCons . fmap (, VNil)

instance MonadTrans (VectorT 1) where
  lift = singleton

infixr `cons`

cons :: (Applicative m) => a -> VectorT n m a -> VectorT (n + 1) m a
cons a = consM $ pure a

runVectorT :: forall m n a . Monad m => VectorT n m a -> m (VectorT n Identity a)
runVectorT = foldr2 VNil cons

toList :: Monad m => VectorT n m a -> m [a]
toList = foldr0 [] (:)

foldr :: Monad m => b 0 -> (forall i . a -> b i -> b (i + 1)) -> VectorT n m a -> m (b n)
foldr start _ VNil = return start
foldr start step (VCons v) = do
  (a, as) <- v
  accum <- foldr start step as
  return $ step a accum

foldr0 :: Monad m => b -> (a -> b -> b) -> VectorT n m a -> m b
foldr0 start step = fmap getConst . foldr (Const start) ((. getConst) . (Const .) . step)

newtype Wrap1 f a n = Wrap1 { getWrap1 :: f n a }

foldr1 :: Monad m => f 0 a -> (forall i . a -> f i a -> f (i + 1) a) -> VectorT n m a -> m (f n a)
foldr1 start step = fmap getWrap1 . foldr (Wrap1 start) ((. getWrap1) . (Wrap1 .) . step)

newtype Wrap2 f x a n = Wrap2 { getWrap2 :: f n x a }

foldr2 :: Monad m => f 0 x b -> (forall i . a -> f i x b -> f (i + 1) x b) -> VectorT n m a -> m (f n x b)
foldr2 start step = fmap getWrap2 . foldr (Wrap2 start) ((. getWrap2) . (Wrap2 .) . step)

foldrM2 :: Monad m => f 0 m b -> (forall i . a -> f i m b -> m (f (i + 1) m b)) -> VectorT n m a -> m (f n m b)
foldrM2 start step = getWrapM2 <=< foldr (WrapM2 $ pure start) (\a WrapM2 { getWrapM2 } -> WrapM2 $ step a =<< getWrapM2)

newtype WrapM2 f m a n = WrapM2 { getWrapM2 :: m (f n m a) }


uncons :: Functor m => VectorT (n + 1) m a -> m (a, VectorT n m a)
uncons (VCons v) = v

append :: Functor m => VectorT n1 m a -> VectorT n2 m a -> VectorT (n1 + n2) m a
append VNil v = v
append (VCons v1) v2 = VCons $ second (append v2) <$> v1

head :: Functor m => VectorT (n + 1) m a -> m a
head = fmap fst . uncons

headMaybe :: Functor m => VectorT n m a -> Maybe (m a)
headMaybe VNil = Nothing
headMaybe (VCons v) = Just $ fst <$> v

data Sing (n :: Natural) where
  Z :: Sing 0
  S :: Sing n -> Sing (n + 1)

replicate :: (KnownNat n, Applicative m) => a -> VectorT n m a
replicate = replicateM . pure

replicateM :: (Functor m, KnownNat n) => m a -> VectorT n m a
replicateM ma = replicateS ma sing

replicateS :: Functor m => m a -> Sing n -> VectorT n m a
replicateS ma = build $ consM ma

build :: (forall i . VectorT i m a -> VectorT (i + 1) m a) -> Sing n -> VectorT n m a
build _ Z = VNil
build increase (S n) = increase $ build increase n

sing :: forall n. KnownNat n => Sing n
sing = case natVal (Proxy @n) of
  0 -> unsafeCoerce Z
  n -> case someNatVal (n - 1) of
        SomeNat p -> unsafeCoerce $ S $ singAt p
 where
  singAt :: KnownNat m => Proxy m -> Sing m
  singAt = const sing

mapM :: Monad m => (a -> m b) -> VectorT n m a -> m (VectorT n m b)
mapM f = foldr2 VNil $ \a v -> VCons $ (, v) <$> f a

data Partition n m a = forall i j . (i + j ~ n) => Partition (VectorT i m a) (VectorT j m a)

deriving instance (Show a, forall x . Show x => Show (m x)) => Show (Partition n m a)

withPartition :: (forall i j . (i + j ~ n) => VectorT i m a -> VectorT j m a -> b n) -> Partition n m a -> b n
withPartition f (Partition v1 v2) = f v1 v2

withPartition2 :: (forall i j . (i + j ~ n) => VectorT i m a -> VectorT j m a -> f n x y) -> Partition n m a -> f n x y
withPartition2 f (Partition v1 v2) = f v1 v2

partition :: Monad m => (a -> Bool) -> VectorT n m a -> m (Partition n m a)
partition _ VNil = pure $ Partition VNil VNil
partition p (VCons v) = do
  (a, as) <- v
  Partition v1 v2 <- partition p as
  return $ if p a
    then Partition (a `cons` v1) v2
    else Partition v1 (a `cons` v2)

data Found n m a where
  Found :: a -> VectorT n m a -> Found (n + 1) m a

deriving instance (Show a, forall x . Show x => Show (m x)) => Show (Found n m a)

find :: Monad m => (a -> Bool) -> VectorT n m a -> m (Either (VectorT n m a) (Found n m a))
find p v = partition p v >>= \case
  (Partition VNil v2) -> return $ Left v2
  (Partition (VCons v1) v2) -> do
    (a, as) <- v1
    return $ Right $ Found a $ append as v2
