module Chapter15SemigroupSpec(spec) where
import Test.Hspec

import Data.Semigroup
import Test.QuickCheck

--data Optional a = Nada | Only a deriving (Eq, Show)

--instance Monoid a => Monoid (Optional a) where
--  mempty                    = Nada
--  mappend Nada (Only x)     = Only ( mappend mempty x)
--  mappend (Only x) Nada     = Only ( mappend x mempty)
--  mappend (Only x) (Only y) = Only ( mappend x y)
--  mappend Nada Nada         = Nada

---------------

--monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
--monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
--
--data Bull = Fools | Twoo deriving (Eq, Show)
--instance Arbitrary Bull where
--  arbitrary = frequency [ (1, return Fools), (1, return Twoo) ]
--
--instance Monoid Bull where
--  mempty = Fools
--  mappend _ _ = Fools
--
--type BullMappend = Bull -> Bull -> Bull -> Bool

---------------1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
-------------------2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentStrings   = (Identity String) -> (Identity String) -> (Identity String) -> Bool

type IdentInts      = (Identity Int) -> (Identity Int) -> (Identity Int) -> Bool
-------------------3
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a <> a') (b <> b')


instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoStrings   = (Two String String) -> (Two String String) -> (Two String String) -> Bool
-------------------4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')


instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeStrings   = (Three String String String) -> (Three String String String) -> (Three String String String) -> Bool
-------------------5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  Four a b c d <> Four a' b' c' d' = Four (a <> a') (b <> b') (c <> c') (d <> d')


instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourStrings   = (Four String String String String) -> (Four String String String String) -> (Four String String String String) -> Bool

-------------------6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = do
    a <- elements [True, False]
    return $ BoolConj a

-------------------7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  _ <> _ = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- elements [True, False]
    return $ BoolDisj a

-------------------8
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Snd a <> _     = Snd a
  _     <> Snd b = Snd b
  _     <> Fst b = Fst b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [ return $ Fst a, return $ Snd b ]

-------------------9

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (f <> g)

f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)

-------------------11

data Validation' a b = Failure' a | Success' b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation' a b) where
  Success' x <> _ = Success' x
  _ <> (Success' x) = (Success' x)
  (Failure' a) <> (Failure' b) = Failure' (a <> b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [ return $ (Success' b), return $ (Failure' a) ]

failure :: String -> Validation' String Int
failure = Failure'
success :: Int -> Validation' String Int
success = Success'
-------------------12

newtype AccumulateRight a b = AccumulateRight (Validation' a b) deriving (Eq, Show)
instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight (Success' x) <> AccumulateRight (Success' y) = AccumulateRight (Success' (x <> y))
  AccumulateRight (Failure' x) <> AccumulateRight (Failure' y) = AccumulateRight (Failure' y)
  AccumulateRight (Failure' x) <> AccumulateRight (Success' y) = AccumulateRight (Failure' x)
  AccumulateRight (Success' x) <> AccumulateRight (Failure' y) = AccumulateRight (Failure' y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [ return $ (AccumulateRight (Success' b)), return $ (AccumulateRight(Failure' a)) ]

failure' :: String -> AccumulateRight String Int
failure' x = AccumulateRight(Failure' x)
success' :: Int -> AccumulateRight String Int
success' x = AccumulateRight(Success' x)

-------------------13
newtype AccumulateBoth a b = AccumulateBoth (Validation' a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (AccumulateBoth (Success' a)) <> (AccumulateBoth (Success' b)) = AccumulateBoth $ Success' (a <> b)
  (AccumulateBoth (Success' _)) <> (AccumulateBoth (Failure' a)) = AccumulateBoth $ Failure' a
  (AccumulateBoth (Failure' a)) <> (AccumulateBoth (Success' _)) = AccumulateBoth $ Failure' a
  (AccumulateBoth (Failure' a)) <> (AccumulateBoth (Failure' b)) = AccumulateBoth $ Failure' (a <> b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [ return $ (AccumulateBoth (Success' b)), return $ (AccumulateBoth(Failure' a)) ]

failure'' :: String -> AccumulateBoth String Int
failure'' x = AccumulateBoth(Failure' x)
success'' :: Int -> AccumulateBoth String Int
success'' x = AccumulateBoth(Success' x)


-------------------
spec :: Spec
spec = do

    describe "Chapter 15" $ do
        --TODO it "BullMappend" $ do property $ (monoidAssoc :: BullMappend)
        it "Semigroup exercises 1" $ do property $ (semigroupAssoc :: TrivAssoc)
        it "Semigroup exercises 2 a" $ do property $ (semigroupAssoc :: IdentStrings)
--        it "Semigroup exercises 2 b" $ do (mappend $ mempty (Identity 1)) == Identity 1

        --TODO it "Semigroup exercises 2 b" $ do property $ (semigroupAssoc :: IdentInts)
        it "Semigroup exercises 3" $ do property $ (semigroupAssoc :: TwoStrings)
        it "Semigroup exercises 4" $ do property $ (semigroupAssoc :: ThreeStrings)
        it "Semigroup exercises 5" $ do property $ (semigroupAssoc :: FourStrings)
        it "Semigroup exercises 6 a" $ do (BoolConj True <> BoolConj True) == BoolConj True
        it "Semigroup exercises 6 b" $ do (BoolConj True <> BoolConj False) == BoolConj False
        it "Semigroup exercises 7 a" $ do (BoolDisj True <> BoolDisj False) == BoolDisj True
        it "Semigroup exercises 7 b" $ do (BoolDisj False <> BoolDisj False) == BoolDisj False
        it "Semigroup exercises 8 a" $ do (Fst 1 <> Snd 2) == Snd 2
        it "Semigroup exercises 8 b" $ do (Fst 1 <> (Fst 2)::Or Int Int) == Fst 2
        it "Semigroup exercises 8 c" $ do (Snd 1 <> Fst 2) == Snd 1
        it "Semigroup exercises 8 d" $ do (Snd 1 <> (Snd 2)::Or Int Int) == Snd 1
        it "Semigroup exercises 9 a" $ do (unCombine (f <> g) $ 0) == Sum {getSum = 0}
        it "Semigroup exercises 9 b" $ do (unCombine (f <> g) $ 10) == Sum {getSum = 20}
        it "Semigroup exercises 9 c" $ do (unCombine (f <> g) $ 42) == Sum {getSum = 84}

        --TODO it "Semigroup exercises 11" $ do (Success' 1 <> Failure' 2) == Success' 1
        it "Semigroup exercises 11 a" $ do (success 1 <> failure "foo") == Success' 1
        it "Semigroup exercises 11 b" $ do (failure "err1" <> failure "err2") == Failure' "err1err2"
        --TODO it "Semigroup exercises 12" $ do (failure' "err1" <> failure' "err2") == Failure' "err1err2"
        --TODO it "Semigroup exercises 13" $ do (failure'' "err1" <> failure'' "err2") == AccumulateBoth (Failure' "err1err2")


