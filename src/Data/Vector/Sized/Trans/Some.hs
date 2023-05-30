module Data.Vector.Sized.Trans.Some where

-- base
import Data.Typeable (cast, Typeable)
import GHC.TypeNats

-- vector-sized-transformer
import Data.Vector.Sized.Trans hiding (singleton, cons, consM)
import qualified Data.Vector.Sized.Trans as VectorT

-- FIXME Applicative
data SomeVectorT m a = forall n . KnownNat n => SomeVectorT { getSomeVectorT :: VectorT n m a }
  deriving Typeable

deriving instance Functor m => Functor (SomeVectorT m)
instance Functor m => Semigroup (SomeVectorT m a) where
  SomeVectorT v1 <> SomeVectorT v2 = SomeVectorT $ append v1 v2
instance Functor m => Monoid (SomeVectorT m a) where
  mempty = SomeVectorT VNil

cons :: (Applicative m) => a -> SomeVectorT m a -> SomeVectorT m a
cons a = consM $ pure a

{- |
Note that it is not possible to construct @consM' :: Functor m => a -> m (SomeVectorT m a) -> SomeVectorT m a@,
because the length in the tail argument is only known after an action in @m@,
while it would be known purely in the result vector.
-}
consM :: Functor m => m a -> SomeVectorT m a -> SomeVectorT m a
consM ma SomeVectorT { getSomeVectorT } = SomeVectorT $ VectorT.consM ma getSomeVectorT

singleton :: Functor m => m a -> SomeVectorT m a
singleton = SomeVectorT . VectorT.singleton

fromList :: Functor m => [m a] -> SomeVectorT m a
fromList = fromFoldable
fromFoldable :: (Functor m, Foldable t) => t (m a) -> SomeVectorT m a
fromFoldable = foldMap singleton

safeHead :: Functor m => SomeVectorT m a -> Maybe (m a)
safeHead SomeVectorT { getSomeVectorT } = VectorT.headMaybe getSomeVectorT

castSomeVectorT :: (Typeable a, Typeable m, KnownNat n) => SomeVectorT m a -> Maybe (VectorT n m a)
castSomeVectorT SomeVectorT { getSomeVectorT } = cast getSomeVectorT

withVectorT :: (forall n . VectorT n m a -> b) -> SomeVectorT m a -> b
withVectorT f SomeVectorT { getSomeVectorT } = f getSomeVectorT
