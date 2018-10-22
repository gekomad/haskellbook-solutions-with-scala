module Chapter17Spec(spec) where
import Test.Hspec
import Control.Applicative
import Data.Monoid

------Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  Identity f <*> Identity a = Identity (f a)
----------------

------Constant
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant b) = Constant b

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant { getConstant = mempty }
  (<*>) (Constant x) (Constant y) = Constant (mappend x y)
----------------

------17.9 1
data Pair a = Pair a a deriving (Show,Eq)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f g) (Pair a b) = Pair (f a) (g b)
----------------

--17.9 2
data Two a b = Two a b deriving (Show,Eq)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two f g) (Two a b) = Two (mappend f a) (g b)

------------Person
validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
    if (length s) > maxLen
    then Nothing
    else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)
mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 6 s
mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address deriving (Eq, Show)

-------------Cow
data Cow = Cow {  name:: String, age:: Int, weight :: Int} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
  | otherwise = Nothing

---------------------------------
spec :: Spec
spec = do

  describe "Chapter 17" $ do
    it "lift" $ do (liftA2 (+) [1, 2] [3, 4]) == [4,5,5,6]
    it "Identity Instance" $ do (Identity (+1) <*> Identity 2) == Identity 3
    it "Constant" $ do (Constant (Sum 1) <*> Constant (Sum 2)) == Constant 3
    it "Lookups a" $ do (pure(+3) <*> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])) == Just 9
    it "17.9 1" $ do (Pair (+1) (*3) <*> Pair 1 2) == Pair 2 6
--    it "17.9 2" $ do (Two (+1) (+1) <*> Two 1 2) == Two 2 3
    it "Person ko" $ do (liftA2 Person (mkName "a wrong looooooooooong name") (Just (Address "farm"))) == Nothing
    it "Person ok" $ do (liftA2 Person (mkName "babe") (Just (Address "farm"))) == (Just (Person (Name "babe") (Address "farm")))
    it "Cow ko" $ do (liftA3 Cow (noEmpty "cow") (noNegative (-4)) (noNegative 100)) == Nothing
    it "Cow ok" $ do (liftA3 Cow (noEmpty "cow") (noNegative 4) (noNegative 100)) == (Just (Cow {name = "cow", age = 4, weight = 100}))


