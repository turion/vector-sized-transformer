module Main (main) where

-- transformers
import Control.Monad.Trans.State

-- hspec
import Test.Hspec

-- vector-sized-transformer
import Data.Vector.Sized.Trans

main :: IO ()
main = hspec $ do
  describe "replicate" $ do
    it "should replicate an action as many times as the type signature says" $ do
      let action = do
            a <- get
            modify (+ 1)
            return a
          vector = replicateM action :: VectorT 4 (State Int) Int
          result = evalState (toList vector) 0
      result `shouldBe` [0, 1, 2, 3]
