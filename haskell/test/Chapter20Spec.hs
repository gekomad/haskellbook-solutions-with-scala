module Chapter20Spec(spec) where
import Test.Hspec

import Data.Monoid
import Data.Foldable

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

foldAllTheThings :: Sum Integer
foldAllTheThings = fold $ fmap (foldMap (+1)) [Two 1 (Sum 3), Two 0 (Sum 1)]

---------------------------------
spec :: Spec
spec = do

  describe "Chapter 20" $ do
    it "Two" $ do foldAllTheThings == Sum {getSum = 6}
