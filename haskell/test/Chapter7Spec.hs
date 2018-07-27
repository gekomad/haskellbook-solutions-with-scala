module Chapter7Spec(spec) where
import Test.Hspec

--Exercises: Variety Pack

k :: (a, b) -> a
k (x, y) = x
k1 = k ((4-1), 10)
k2 :: [Char]
k2 = k ("three", (1 + 2))
k3 :: Int
k3 = k (3, True)

------------------------------------

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f(a, _, c) (d, _, f) = ((a, d), (c, f))

-------------------------------------
functionC x y = if (x > y) then x else y

functionCcase x y = case cool of
        True -> x
        False -> y
        where cool = x > y

-------------------------------------

ifEvenAdd2 n = if even n then (n+2) else n

ifEvenAdd2case n = case even n of
        True -> n + 2
        False -> n

nums x = case compare x 0 of
        LT -> -1
        GT -> 1
        _ -> 0

-------------------Artful Dodgy------

--flip :: (a -> b -> c) -> b -> a -> c
--flip f x y = f y x

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

------------------Let’s write code----------------
--1.
tensDigit :: Integral a => a -> a
tensDigit x = d where
        xLast = x `div` 10
        d = xLast `mod` 10

--1. a
tensDigit2 :: Integral a => a -> a
tensDigit2 x = d where
        (a,_) = x `divMod` 10
        d = a `mod` 10

--1. c
hunsD :: Integral a => a -> a
hunsD x = d2 where
        d2 = x `div` 100
        d = d `mod` 10

--2.
--case expression
foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y b =
    case b of
        True -> x
        False -> y

--guard
foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y b
    | b == True = x
    | otherwise = y

--3 Fill in the definition

g :: ( Num a, Num b, Num c) => (a -> b) -> (a, c) -> (b, c)
--g f x = (f (fst x),snd x)
g f (a,b) = (t,b)
    where t = f a

--5
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripPointFree :: (Show a, Read a) => a -> a
roundTripPointFree = read . show

--6
roundTrip6 :: (Show a, Read b) => a -> b
roundTrip6 a = read (show a)

roundTripPointFree6 :: (Show a, Read b) => a -> b
roundTripPointFree6 = read . show

-------------------------------------------------
spec :: Spec
spec = do

  describe "Chapter 7" $ do

    it "k1 == 3" $ do k1 == 3

    it "k2 == three" $ do k2 == "three"

    it "k3 == 3" $ do k3 == 3

-- Remember: Tuples have the same syntax for their
-- type constructors and their data constructors.

    it "tuples" $ do f(1,2,3)(4,5,6) == ((1,4),(3,6))

    it "coolness 1" $ do functionC 1 2 ==  functionCcase 1 2 && functionC 1 2 == 2

    it "coolness 2" $ do functionC 3 2 ==  functionCcase 3 2 && functionC 3 2 == 3

    it "ifEvenAdd2" $ do (ifEvenAdd2case 2) == ifEvenAdd2 2 && ifEvenAdd2case 2 == 4

    it "ifEvenAdd2bis" $ do ifEvenAdd2case 3 ==  ifEvenAdd2 3 && ifEvenAdd2case 3 == 3

    it "nums" $ do nums 3 == 1

    it "nums" $ do nums 0 == 0

    it "nums" $ do nums (-10) == -1

-------------------Artful Dodgy------

    it "dodgy 1 1" $ do dodgy 1 1 == 11

    it "dodgy 2 2" $ do dodgy 2 2 == 22

    it "dodgy 1 2" $ do dodgy 1 2 == 21

    it "dodgy 2 1" $ do dodgy 2 1 == 12

    it "oneIsOne 1" $ do oneIsOne 1 == 11

    it "oneIsOne 2" $ do oneIsOne 2 == 21

    it "oneIsTwo 1" $ do oneIsTwo 1 == 21

    it "oneIsTwo 2" $ do oneIsTwo 2 == 22

    it "oneIsOne 3" $ do oneIsOne 3 == 31

    it "oneIsTwo 3" $ do oneIsTwo 3 == 23

--1. a-c

    it "tensDigit 1" $ do tensDigit 1 == tensDigit2 1

    it "tensDigit 2" $ do tensDigit 10 == tensDigit2 10

    it "tensDigit 3" $ do tensDigit 111 == tensDigit2 111

--2.

    it "case expression/guard true" $ do foldBoolCase 1 3 True == foldBoolGuard 1 3 True && foldBoolCase 1 3 True == 1

    it "case expression/guard false" $ do foldBoolCase 1 3 False == foldBoolGuard 1 3 False && foldBoolCase 1 3 False == 3

--3.

    it "3 Fill in the definition" $ do g (\x -> x + 2) (1,2) == (3,2)

--5
    it "5 roundTrip" $ do show( roundTripPointFree (4)) == show (roundTrip 4) && show( roundTripPointFree (4)) == "4"

--6
    it "6 roundTrip int" $ do show( roundTripPointFree6 4 ::Int) == show (roundTrip6 4::Int) && show( roundTripPointFree6 4::Int) == "4"

    it "6 roundTrip float" $ do show( roundTripPointFree6 4.0 ::Float) == show (roundTrip6 4.0::Float) && show( roundTripPointFree6 4.0::Float) == "4.0"