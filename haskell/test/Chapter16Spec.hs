{-# LANGUAGE FlexibleInstances #-}
module Chapter16Spec(spec) where
import Test.Hspec
import Data.Functor
import Test.QuickCheck

data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

---------------------------------
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f
functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

---------------------------------1
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

plus :: Num i => i -> (Identity i) -> (Identity i)
plus x (Identity i)  = Identity (x + i)
---------------------------------2
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)


plus2 :: Num i => i -> (Pair i) -> (Pair i)
plus2 x (Pair i y) = Pair i (y + x)
---------------------------------3
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

plus3 :: Num i => i -> (Two a i) -> (Two a i)
plus3 x (Two i y) = Two i (y + x)
---------------------------------4
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

plus4 :: Num c => c -> (Three a b c) -> (Three a b c)
plus4 x (Three a b c) = Three a b (c + x)

---------------------------------5
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b ) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

plus5 :: Num b => b -> (Three' a b) -> (Three' a b)
plus5 x (Three' a b b') = Three' a (b + x) (b' + x)

---------------------------------6
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

plus6 :: Num d => d -> (Four a b c d) -> (Four a b c d)
plus6 x (Four a b c d) = Four a b c (d + x)

---------------------------------7
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a1 a2 b) = Four' a a1 a2 (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Four' a a a b

plus7 :: Num d => d -> (Four' a d) -> (Four' a d)
plus7 x (Four' a b c d) = Four' a b c (d + x)
---------------------------------
data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers $ f a

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = do
    a <- arbitrary
    return $ Yeppers a

---------------------------------

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

---------------------------------1
data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)
instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor a) = Bloor $ f a
---------------------------------2

data K a b = K a deriving (Eq, Show)
instance Functor (K a) where
  fmap _ (K a) = K a
---------------------------------3

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip $ K (f b)
---------------------------------4

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b
---------------------------------
spec :: Spec
spec = do

  describe "Chapter 16" $ do
    it "FixMePls a" $ do (fmap (\n -> n+1) $ Pls 2) == Pls 3
    it "FixMePls b" $ do (fmap (\_ -> undefined::Int) FixMe) == FixMe
    it "functorIdentity" $ do property $ (\x -> functorIdentity (x :: [Int]) )
    it "functorCompose" $ do property $ (\x -> functorCompose (+1) (*2) (x :: [Int]) )
    it "Instances of Func 1 id" $ do property $ (\x -> functorIdentity (x :: [Identity Int]) )
    it "Instances of Func 1 comp" $ do property $ (\x -> functorCompose (plus 1) (plus 2) (x :: [Identity Int]) )
    it "Instances of Func 2 id" $ do property $ (\x -> functorIdentity (x :: [Pair Int]) )
    it "Instances of Func 2 comp" $ do property $ (\x -> functorCompose (plus2 1) (plus2 2) (x :: [Pair Int]) )
    it "Instances of Func 3 id" $ do property $ (\x -> functorIdentity (x :: [Two Int Int]) )
    it "Instances of Func 3 comp" $ do property $ (\x -> functorCompose (plus3 1) (plus3 3) (x :: [Two String Int]) )
    it "Instances of Func 4 id" $ do property $ (\x -> functorIdentity (x :: [Three Int Int Int]) )
    it "Instances of Func 4 comp" $ do property $ (\x -> functorCompose (plus4 1) (plus4 3) (x :: [Three String String Int]) )
    it "Instances of Func 5 id" $ do property $ (\x -> functorIdentity (x :: [Three' Int Int]) )
    it "Instances of Func 5 comp" $ do property $ (\x -> functorCompose (plus5 1) (plus5 3) (x :: [Three' String Int]) )
    it "Instances of Func 6 id" $ do property $ (\x -> functorIdentity (x :: [Four String String String Int]) )
    it "Instances of Func 6 comp" $ do property $ (\x -> functorCompose (plus6 1) (plus6 3) (x :: [Four String String Char Int]) )
    it "Instances of Func 7 id" $ do property $ (\x -> functorIdentity (x :: [Four' String Int]) )
    it "Instances of Func 7 comp" $ do property $ (\x -> functorCompose (plus7 1) (plus7 3) (x :: [Four' String Int]) )
    it "Possibly id" $ do property $ (\x -> functorIdentity (x :: [Possibly Int]) )
    it "Possibly a" $ do (fmap (+1) (Yeppers 1)) == (Yeppers 2)
    it "Possibly b" $ do (fmap (+1) LolNope) == LolNope
    it "Sum a" $ do (fmap (+1) (First "err")) == First "err"
    it "Sum b" $ do ((fmap (+1) (Second 1))::(Sum Int Int)) == Second 2