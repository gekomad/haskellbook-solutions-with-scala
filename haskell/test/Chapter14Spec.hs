module Chapter14Spec(spec) where

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do

    describe "Chapter 14" $ do

        it "3 - plusAssociative" $ do property $ \x y z -> x + (y + z) == ((x :: Int) + (y :: Int)) + (z :: Int)
        it "3 - plusCommutative" $ do property $ \x y -> x + y == (y :: Int) + (x :: Int)

        it "4 - mulAssociative" $ do property $ \x y z -> x * (y * z) == ((x :: Int) * (y :: Int)) * (z :: Int)
        it "4 - mulCommutative" $ do property $ \x y -> x * y == (y :: Int) * (x :: Int)