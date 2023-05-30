{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Data.Vector.Sized.Trans where

-- base
import Control.Monad ((>=>))
import Data.Functor.Identity
import Data.Proxy
import GHC.TypeNats
import Unsafe.Coerce (unsafeCoerce)

-- transformers
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Functor ((<&>))

-- mmorph
import Control.Monad.Morph (MFunctor (..))
import Prelude hiding (mapM, traverse)

data VectorT' (n :: Nat) m a where
  VNil :: VectorT' 0 m a
  VCons :: a -> VectorT n m a -> VectorT' (n + 1) m a

data VectorT'' (n :: Nat) m a where
  VNil' :: VectorT'' 0 m a
  VCons' :: m (a, VectorT'' n m a) -> VectorT'' (n + 1) m a

cons'' :: Functor m => m a -> VectorT'' n m a -> VectorT'' (n + 1) m a
cons'' ma mas = VCons' $ ( , mas) <$> ma

-- FIXME look at all the classes vector-sized has and get them as well

newtype VectorT (n :: Nat) m a = VectorT { getVectorT :: m (VectorT' n m a) }

deriving instance ((forall x . Show x => Show (m x)), Show a) => Show (VectorT n m a)
deriving instance ((forall x . Show x => Show (m x)), Show a) => Show (VectorT' n m a)

deriving instance ((forall x . Show x => Show (m x)), Show a) => Show (VectorT n m a)
deriving instance ((forall x . Show x => Show (m x)), Show a) => Show (VectorT' n m a)

deriving instance Functor m => Functor (VectorT'' n m)
deriving instance Functor m => Functor (VectorT' n m)
deriving instance Functor m => Functor (VectorT n m)

deriving instance Foldable m => Foldable (VectorT' n m)
deriving instance Foldable m => Foldable (VectorT n m)
deriving instance (Functor m, Foldable m, Traversable m) => Traversable (VectorT' n m)
deriving instance (Functor m, Foldable m, Traversable m) => Traversable (VectorT n m)

instance MFunctor (VectorT n) where
  hoist morph VectorT { getVectorT } = VectorT { getVectorT = morph $ hoist morph <$> getVectorT }

instance MFunctor (VectorT' n) where
  hoist _ VNil = VNil
  hoist morph (VCons a as) = VCons a $ hoist morph as

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

uncons :: Functor m => VectorT (n + 1) m a -> m (a, VectorT n m a)
uncons VectorT { getVectorT } = getVectorT <&> \case VCons a v -> (a, v)

infixr `cons`

cons :: Functor m => m a -> VectorT n m a -> VectorT (n + 1) m a
cons ma as = VectorT $ (`VCons` as) <$> ma

cons' :: (Functor m, Applicative m) => m a -> VectorT' n m a -> m (VectorT' (n + 1) m a)
cons' ma as = (`VCons` (VectorT $ pure as)) <$> ma

empty :: Applicative m => VectorT 0 m a
empty = VectorT $ pure VNil

singleton :: Monad m => m a -> VectorT 1 m a
singleton = lift

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

replicateS' :: (Functor m, Monad m) => m a -> Sing n -> m (VectorT' n m a)
replicateS' _ Z = pure VNil
replicateS' ma (S n) = cons' ma =<< replicateS' ma n

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

mapM :: Monad m => (a -> m b) -> VectorT n m a -> VectorT n m b
mapM f VectorT { getVectorT } = VectorT $ mapM' f =<< getVectorT

mapM' :: Monad m => (a -> m b) -> VectorT' n m a -> m (VectorT' n m b)
mapM' _ VNil = pure VNil
mapM' f (VCons a as) = VCons <$> f a <*> pure (mapM f as)

findFirst :: Monad m => (a -> Bool) -> VectorT n m a -> m (Either (VectorT n m a) (Found n m a))
findFirst p VectorT { getVectorT } = getVectorT >>= findFirst' p
findFirst' :: Monad m => (a -> Bool) -> VectorT' n m a -> m (Either (VectorT n m a) (Found n m a))
findFirst' _ VNil = pure $ Left $ VectorT $ pure VNil
-- FIXME Rewrite with fmap
findFirst' p (VCons a as) = if p a then pure $ Right $ Found a as else do
  foundMaybe <- findFirst p as
  case foundMaybe of
    Left as' -> pure $ Left $ pure a `cons` as'
    Right (Found a' as') -> pure $ Right $ Found a' $ VectorT $ pure $ VCons a as'

data Found n m a where
  Found :: a -> VectorT n m a -> Found (n + 1) m a

data Split n m a = forall i j . (i + j ~ n) => Split (VectorT i m a) (VectorT j m a)

glue :: Applicative m => Split n m a -> VectorT n m a
glue (Split v1 v2) = append v1 v2

deriving instance (Show a, forall x . Show x => Show (m x)) => Show (Found n m a)
