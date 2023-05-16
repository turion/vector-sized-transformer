{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module Data.Vector.Sized.Trans where

-- base
import Control.Monad ((>=>))
import Data.Functor.Identity
import Data.Proxy
import GHC.TypeNats
import Unsafe.Coerce (unsafeCoerce)

-- transformers
import Control.Monad.Trans.Class (MonadTrans (..))

data VectorT' (n :: Nat) m a where
  VNil :: VectorT' 0 m a
  VCons :: a -> VectorT n m a -> VectorT' (n + 1) m a

newtype VectorT (n :: Nat) m a = VectorT { getVectorT :: m (VectorT' n m a) }

deriving instance Functor m => Functor (VectorT' n m)
deriving instance Functor m => Functor (VectorT n m)

instance MonadTrans (VectorT 1) where
  lift = VectorT . fmap (`VCons` VectorT (pure VNil))

runVectorT :: Monad m => VectorT n m a -> m (VectorT n Identity a)
runVectorT VectorT { getVectorT } = do
  vector' <- getVectorT
  VectorT . pure <$> runVectorT' vector'

runVectorT' :: Monad m => VectorT' n m a -> m (VectorT' n Identity a)
runVectorT' VNil = pure VNil
runVectorT' (VCons a mas) = VCons a <$> runVectorT mas

toList :: Monad m => VectorT n m a -> m [a]
toList = getVectorT >=> toList'

toList' :: (Monad m) => VectorT' n m a -> m [a]
toList' VNil = pure []
toList' (VCons a as) = (a :) <$> toList as

cons :: Functor m => m a -> VectorT n m a -> VectorT (n + 1) m a
cons ma as = VectorT $ (`VCons` as) <$> ma

append :: Applicative m => VectorT n1 m a -> VectorT n2 m a -> VectorT (n1 + n2) m a
append v1 v2 = VectorT $ append'' <$> getVectorT v1 <*> getVectorT v2

-- FIXME inline?
append' :: Functor m => VectorT n1 m a -> VectorT' n2 m a -> VectorT (n1 + n2) m a
append' VectorT { getVectorT } v2 = VectorT $ (`append''` v2) <$> getVectorT

append'' :: Functor m => VectorT' n1 m a -> VectorT' n2 m a -> VectorT' (n1 + n2) m a
append'' VNil v = v
append'' (VCons a v1) v2 = VCons a $ append' v1 v2


head' :: VectorT' n m a -> Maybe a
head' VNil = Nothing
head' (VCons a _) = Just a

headSafe' :: VectorT' (n + 1) m a -> a
headSafe' (VCons a _) = a

headSafe :: Functor m => VectorT (n + 1) m a -> m a
headSafe = fmap headSafe' . getVectorT

replicateM :: (KnownNat n, Applicative m) => m a -> VectorT n m a
replicateM ma = replicateS ma sing

data Sing (n :: Natural) where
  Z :: Sing 0
  S :: Sing n -> Sing (n + 1)

replicateS :: Applicative m => m a -> Sing n -> VectorT n m a
replicateS _ Z = VectorT $ pure VNil
replicateS ma (S n) = ma `cons` replicateS ma n

sing :: forall n. KnownNat n => Sing n
sing = case natVal (Proxy @n) of
  0 -> unsafeCoerce Z
  n -> case someNatVal (n - 1) of
        SomeNat p -> unsafeCoerce $ S $ singAt p
 where
  singAt :: KnownNat m => Proxy m -> Sing m
  singAt = const sing

push :: Monad m => m (VectorT n m a) -> VectorT n m a
push mas = VectorT $ do
  VectorT { getVectorT } <- mas
  getVectorT
