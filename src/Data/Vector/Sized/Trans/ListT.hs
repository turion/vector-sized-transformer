module Data.Vector.Sized.Trans.ListT where

-- base
import Control.Monad (join)

-- transformers
import Control.Monad.Trans.Class (lift)

-- list-t
import ListT hiding (cons)
import ListT qualified

-- vector-sized-transformer
import Data.Vector.Sized.Trans.Some
import Data.Vector.Sized.Trans hiding (cons)

fromListT :: Monad m => ListT m a -> m (SomeVectorT m a)
fromListT = fold (\as a -> pure (pure a `cons` as)) mempty

toListT :: Monad m => VectorT n m a -> ListT m a
toListT = join . lift . fmap toListT' . getVectorT

toListT' :: Monad m => VectorT' n m a -> ListT m a
toListT' VNil = mempty
toListT' (VCons a as) = ListT.cons a $ toListT as
