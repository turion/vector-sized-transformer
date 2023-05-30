-- transformers
import Control.Monad.Trans.State

-- hspec
import Test.Hspec

-- vector-sized-transformer
import Data.Vector.Sized.Trans
import Data.Functor.Identity

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

  describe "find" $ do
    it "should find the value in the head" $ do
      let vector = pure 0 `cons` pure 1 `cons` pure 2 `cons` empty :: VectorT 3 IO Int
      Right (Found n _) <- findFirst even vector
      n `shouldBe` 0
    it "should find the value in the tail" $ do
      let vector = pure 1 `cons` pure 1 `cons` pure 2 `cons` empty :: VectorT 3 IO Int
      Right (Found n _) <- findFirst even vector
      n `shouldBe` 2
    it "should return the original vector when there is no value" $ do
      let vector = pure 1 `cons` pure 1 `cons` pure 1 `cons` empty :: VectorT 3 Identity Int
      runIdentity (findFirst even vector) `shouldBe` Left vector
    it "should return empty for an empty vector" $ do
      runIdentity (findFirst even empty) `shouldBe` _
