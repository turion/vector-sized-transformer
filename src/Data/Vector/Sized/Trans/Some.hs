module Data.Vector.Sized.Trans.Some where

-- base
import Data.Typeable (cast, Typeable)
import GHC.TypeNats

-- vector-sized-transformer
import Data.Vector.Sized.Trans hiding (singleton, cons)

-- FIXME Applicative
data SomeVectorT m a = forall n . KnownNat n => SomeVectorT { getSomeVectorT :: VectorT n m a }
  deriving Typeable

deriving instance Functor m => Functor (SomeVectorT m)

instance Applicative m => Semigroup (SomeVectorT m a) where
  SomeVectorT v1 <> SomeVectorT v2 = SomeVectorT $ append v1 v2

instance Applicative m => Monoid (SomeVectorT m a) where
  mempty = SomeVectorT $ VectorT $ pure VNil

cons :: Functor m => m a -> SomeVectorT m a -> SomeVectorT m a
cons ma SomeVectorT { getSomeVectorT } = SomeVectorT $ VectorT $ flip VCons getSomeVectorT <$> ma

singleton :: (Applicative m) => m a -> SomeVectorT m a
singleton ma = cons ma mempty

fromList :: Applicative m => [m a] -> SomeVectorT m a
fromList = fromFoldable

fromFoldable :: (Applicative m, Foldable t) => t (m a) -> SomeVectorT m a
fromFoldable = foldMap singleton

safeHead :: Functor m => SomeVectorT m a -> m (Maybe a)
safeHead SomeVectorT { getSomeVectorT } = head' <$> getVectorT getSomeVectorT


castSomeVectorT :: (Typeable a, Typeable m, KnownNat n) => SomeVectorT m a -> Maybe (VectorT n m a)
castSomeVectorT SomeVectorT { getSomeVectorT } = cast getSomeVectorT

withVectorT :: (forall n . VectorT n m a -> b) -> SomeVectorT m a -> b
withVectorT f SomeVectorT { getSomeVectorT } = f getSomeVectorT
