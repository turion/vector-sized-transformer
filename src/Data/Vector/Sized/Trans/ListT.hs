module Data.Vector.Sized.Trans.ListT where

-- list-t
import ListT hiding (cons)

-- vector-sized-transformer
import Data.Vector.Sized.Trans.Some

fromListT :: Monad m => ListT m a -> m (SomeVectorT m a)
fromListT = fold (\as a -> pure (pure a `cons` as)) mempty
