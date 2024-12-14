module Data.Vector.Sized.Trans.ListT where

-- base
import Control.Arrow (second)

-- list-t
import ListT hiding (cons)

-- vector-sized-transformer
import Data.Vector.Sized.Trans hiding (cons)
import Data.Vector.Sized.Trans.Some

{- |
Note that it is not possible to relax the constraint to @'Functor' m@,
although one might believe so,
because the result vector knows its length after one action in @m@,
while the argument of type @'ListT' m a@ only knows it after it has traversed the whole list.
Hence, all the steps of it must be joined first.
-}
fromListT :: Monad m => ListT m a -> m (SomeVectorT m a)
fromListT = fold (\as a -> return $ cons a as) mempty

{- |
Actually, 'Applicative' is only used for the construction of the empty list
-}
toListT :: Applicative m => VectorT n m a -> ListT m a
toListT VNil = ListT $ pure Nothing
toListT (VCons v) = ListT $ Just . second toListT <$> v
