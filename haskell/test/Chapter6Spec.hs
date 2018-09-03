module Chapter6Spec(spec) where
import Test.Hspec

data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun
--     deriving (Eq, Show)


instance Eq DayOfWeek where
    (==) Mon Mon    = True
    (==) Tue Tue    = True
    (==) Weds Weds  = True
    (==) Thu Thu    = True
    (==) Fri Fri    = True
    (==) Sat Sat    = True
    (==) Sun Sun    = True
    (==) _ _        = False

instance Show DayOfWeek where
    show Mon     = "Mon"
    show Tue     = "Tue"
    show Weds    = "Weds"
    show Thu     = "Thu"
    show Fri     = "Fri"
    show Sat     = "Sat"
    show Sun     = "Sun"

data Date = Date DayOfWeek Int

instance Eq Date where
    (==) (Date weekday dayOfMonth)
         (Date weekday' dayOfMonth') =
         weekday == weekday' && dayOfMonth == dayOfMonth'

instance Show Date where
    show (Date weekday dayOfMonth) = show weekday ++ " " ++ show dayOfMonth

---------------------------------------

data Identity a = Identity a
instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'
----------------------------------------


-- Write the Eq instance for the datatype provided

-- 1
data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
    (==) (TisAn x) (TisAn y) = x == y

-- 2
data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
    (==) (Two x' y') (Two x'' y'') = x' == x'' && y' == y''

-- 3
data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt b) = a == b
    (==) (TisAString a) (TisAString b) = a == b
    (==) _ _ = False

-- 4
data Pair a = Pair a a
instance Eq a => Eq(Pair a) where
    (==) (Pair x' y') (Pair x'' y'') = x' == x'' && y' == y''

-- 5
data Tuple a b = Tuple a b
instance (Eq a,Eq b) => Eq(Tuple a b) where
    (==) (Tuple x' y') (Tuple x'' y'') = x' == x'' && y' == y''

-- 6
data Which a = ThisOne a | ThatOne a
instance Eq a => Eq(Which a) where
    (==) (ThisOne x) (ThisOne y) = x == y
    (==) (ThatOne x) (ThatOne y) = x == y
    (==) (ThatOne x) (ThisOne y) = x == y
    (==) (ThisOne x) (ThatOne y) = x == y

-- 7
data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq(EitherOr a b) where
    (==) (Hello x) (Hello y) = x == y
    (==) (Goodbye x) (Goodbye y) = x == y
    (==) _ _ = False

-- 1
data Person = Person Bool
    deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

stringPerson :: Person -> String
stringPerson person = show person

--2

data Mood = Blah | Woot  deriving (Show, Eq)

settleDown x = if x == Woot then Blah else x

spec :: Spec
spec = do
  describe "Chapter 6" $ do

    it "mon == mon" $ do Mon == Mon

    it "Date show" $ do show(Date Fri 8) `shouldBe` "Fri 8"

    it "Date eq" $ do Date Mon 1 `shouldBe` Date Mon 1

    it "Date not eq" $ do Date Mon 1 `shouldNotBe` Date Mon 2

--1
    it "TisAn 1 == TisAn 1" $ do (TisAn 1) == (TisAn 1)
--2
    it "Two 1 2 == Two 1 1" $ do (Two 1 2) /= (Two 1 1)
--3
    it "TisAnInt 1 == TisAnInt 1" $ do (TisAnInt 1) == (TisAnInt 1 )

    it "TisAnInt \"a\" == TisAnInt \"b\"" $ do (TisAString "a") /= (TisAString "b" )

--4
    it "Pair 1 2 == Pair 1 1" $ do Pair 1 2 /= Pair 1 1

    it "Pair \"1\" \"2\"  == Pair \"1\" \"2\"" $ do (Pair "1" "2") == (Pair "1" "2")

--5
    it "Tuple 1 \"2\" /= Tuple 1 \"1\" " $ do (Tuple 1 "2" ) /= (Tuple 1 "1")

    it "Tuple 1 \"2\" == Tuple 1 \"2\" " $ do (Tuple "1" "2" ) == (Tuple "1" "2")

--6
    it "ThisOne 1 == ThisOne 1" $ do ThisOne 1 == ThisOne 1

    it "ThatOne \"1\" == ThatOne \"1\"" $ do ThatOne "1" == ThatOne "1"

    it "ThisOne 1 == ThatOne 1" $ do ThisOne 1 == ThatOne 1

    it "ThatOne \"1\" == ThisOne \"1\"" $ do ThatOne "1" == ThisOne "1"

--7 TODO
--    it ("Hello 1 == Hello 1") $ do Hello 1 == Hello 1
--    it "Goodbye \"1\" == Goodbye \"1\"" $ do (Goodbye "1") == (Goodbye "1")

--1

    it "stringPerson (Person True)" $ do stringPerson (Person True) `shouldBe` "Person True"

--2
    it "settleDown Blah" $ do show(settleDown Blah) == "Blah"

    it "settleDown Woot" $ do show(settleDown Woot) == "Blah"

--3
    it "a) What values are acceptable inputs to that function? -> Woot and Blah" $ do 1 == 1

    it "b) What will happen if you try to run settleDown 9? Why? -> it goes in error because doesn't implement Num" $ do 1 == 1

    it "c) What will happen if you try to run Blah > Woot? Why? -> it goes in error because Ord is missing" $ do 1 == 1