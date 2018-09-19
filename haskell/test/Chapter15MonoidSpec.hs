module Chapter15MonoidSpec(spec) where
import Test.Hspec
import Data.Monoid
import Test.QuickCheck

---------------1
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-------------------2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentStrings = (Identity String) -> (Identity String) -> (Identity String) -> Bool

-------------------3
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mappend = (<>)
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoStrings   = (Two String String) -> (Two String String) -> (Two String String) -> Bool

-------------------4
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mappend = (<>)
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = do
    a <- elements [True, False]
    return $ BoolConj a
-------------------5
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mappend = (<>)
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- elements [True, False]
    return $ BoolDisj a
-------------------6

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (f <> g)

instance Monoid b => Monoid (Combine a b) where
  mappend = (<>)
  mempty = Combine mempty

f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)
-------------------7
newtype Comp a = Comp (a -> a)

instance Semigroup a => Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f <> g)


instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp mempty
  mappend = (<>)

f1 = \n -> n + 1

-------------------
spec :: Spec
spec = do

    describe "Chapter 15" $ do

        it "Monoid exercises 1" $ do property $ (semigroupAssoc :: TrivialAssoc)
        it "Monoid exercises 2" $ do property $ (semigroupAssoc :: IdentStrings)
        it "Monoid exercises 3" $ do property $ (semigroupAssoc :: TwoStrings)
        it "Monoid exercises 3" $ do property $ (semigroupAssoc :: TwoStrings)

        it "Monoid exercises 4 a" $ do (BoolConj True <> BoolConj True) == BoolConj True
        it "Monoid exercises 4 b" $ do (BoolConj True <> BoolConj False) == BoolConj False
        it "Monoid exercises 4 c" $ do ((BoolConj True) `mappend` mempty) == BoolConj True
        it "Monoid exercises 4 d" $ do (mempty `mappend` (BoolConj False)) == BoolConj False

        it "Monoid exercises 5 a" $ do (BoolDisj True <> BoolDisj False) == BoolDisj True
        it "Monoid exercises 5 b" $ do (BoolDisj False <> BoolDisj False) == BoolDisj False
        it "Monoid exercises 5 c" $ do ((BoolDisj True) `mappend` mempty) == BoolDisj True
        it "Monoid exercises 5 d" $ do (mempty `mappend` (BoolDisj False)) == BoolDisj False

        it "Monoid exercises 6" $ do (unCombine (mappend f mempty) $ 1) == Sum {getSum = 2}
--        it "Monoid exercises 7" $ do (Comp $ mappend f1 mempty) $ 1) == Sum {getSum = 2}
      