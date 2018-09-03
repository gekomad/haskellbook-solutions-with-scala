module Chapter10Spec(spec) where
import Test.Hspec

import Data.Time

data DatabaseItem = DbString String
                    | DbNumber Integer
                    | DbDate   UTCTime
                    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate d1
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate d2
    ]

-----
d1 = UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)
d2 = UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)
d3 = UTCTime (fromGregorian 2003 5 1) (secondsToDiffTime 34123)
d4 = UTCTime (fromGregorian 2002 5 1) (secondsToDiffTime 34123)

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate ((DbDate x) : xs) = x : (filterDbDate xs)
filterDbDate (_:xs) = filterDbDate xs
-------------------------------------------

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber ((DbNumber x) : xs) = x : (filterDbNumber xs)
filterDbNumber (_:xs) = filterDbNumber xs
-------------------------------------------

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = go $ filterDbDate xs
    where go :: [UTCTime] -> UTCTime
          go [] = error "NoSuchElement"
          go (x1:xs1) = foldl max x1 xs1
--------------------------

avgDb :: [DatabaseItem] -> Double
avgDb xs = go (filterDbNumber xs)
    where go xsn = (fromIntegral $ sum xsn) / (fromIntegral $ length xsn)
------------------------------
fibs = 1 : scanl (+) 1 fibs
-------------------------------

factorial :: (Num b, Enum b) => [b]
factorial = scanl (*) 1 [1..]
-------------------------------

seekritFunc :: String -> Int
seekritFunc x =
    div (sum (map length (words x)))
        (length (words x))
-------------------------------

stops = "pbtdkg"
vowels = "aeiou"

nouns = ["tree", "sea", "bird"]
verbs = ["go", "buy", "walk"]

-------------------------------
myOr :: [Bool] -> Bool
myOr b = foldr (||) False b
infiniteTrue = True : infiniteTrue

-------------------------------
myAny :: (a -> Bool) -> [a] -> Bool
myAny f b = foldr (\x y -> y || f(x) ) False b
-------------------------------
myElem :: Eq a => a -> [a] -> Bool
myElem a b = foldr (\x acc -> x == a || acc ) False b
-------------------------------
myReverse :: [a] -> [a]
myReverse b = foldr (\x y -> y ++ [x]) [] b
-------------------------------
myMap :: (a -> b) -> [a] -> [b]
myMap f b = foldr (\x acc -> (f x) : acc ) [] b
-------------------------------
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f b = foldr (\ x acc -> if (f x) then x : acc else acc) [] b
-------------------------------
squish :: [[a]] -> [a]
squish b =  foldr (\ x acc -> x ++ acc) [] b
-------------------------------
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f b = foldr (\ x acc -> f x ++ acc) [] b
-------------------------------
squishAgain :: [[a]] -> [a]
squishAgain b = squishMap (\x -> x) b
-------------------------------

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy f b = foldl (\ x acc -> if f x acc == GT then x else acc) (head b) b
-------------------------------
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = undefined
myMinimumBy f b = foldl (\ x acc -> if f x acc == LT then x else acc) (head b) b
-------------------------------
spec :: Spec
spec = do

  describe "Chapter 10" $ do
    it "Understanding Folds 5.a" $ do foldr (++) [] ["woot", "WOOT", "woot"] == "wootWOOTwoot"

    it "Understanding Folds 5.b" $ do foldr max [] (words "fear is the little death") == "the"

    it "Understanding Folds 5.c" $ do foldr (&&) True [False, True] == False

    it "Understanding Folds 5.d" $ do foldr (||) True [False, True] == True

    it "Understanding Folds 5.e" $ do foldr ((++) . show) "" [1..5] == "12345"

    it "Understanding Folds 5.f" $ do foldr (\_ -> const 'a') ' '  [1..5] == 'a'

    it "Understanding Folds 5.g" $ do foldr (\_ -> const 0) 0  "tacos" == 0

    it "Understanding Folds 5.h" $ do foldr (flip const ) 0 "burritos" == 0

    it "Understanding Folds 5.i" $ do foldr (flip const) 'z' [1..5] == 'z'

    it "Database Processing 1" $ do filterDbDate theDatabase == [d1, d2]

    it "Database Processing 2" $ do filterDbNumber theDatabase == [9001]

    it "Database Processing 3" $ do mostRecent theDatabase == d2

    it "Database Processing 4" $ do (sum . filterDbNumber $ theDatabase) == 9001

    it "Database Processing 5" $ do (avgDb theDatabase) == 9001

    it "Scans Exercises 1" $ do (take 20 fibs) == [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765]

    it "Scans Exercises 2" $ do (takeWhile (<100) fibs) == [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89]

    it "Scans Exercises 3" $ do take(10) factorial == [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]

    it "Chapter Exercises 1 a" $ do (length [(a,b,c) | a <- stops, b <- vowels, c <- stops ]) == 180

    it "Chapter Exercises 1 b" $ do (length [(a,b,c) | a <- stops, b <- vowels, c <- stops , a == 'p']) == 30

    it "Chapter Exercises 1 c" $ do (length [(a,b,c) | a <- nouns, b <- verbs, c <- nouns ]) == 27

    it "Chapter Exercises 2 - seekritFunc" $ do (seekritFunc "aa bb ccc ddddd") == 3

    it "Rewriting functions using folds - 1 - myOr (1)" $ do (myOr [True,True]) == True
    it "Rewriting functions using folds - 1 - myOr (2)" $ do (myOr [False,False]) == False
    it "Rewriting functions using folds - 1 - myOr (3)" $ do (myOr [True,False]) == True
    it "Rewriting functions using folds - 1 - myOr (4)" $ do (myOr [False,True,False]) == True
    it "Rewriting functions using folds - 1 - myOr (5)" $ do (myOr [False,True]) == True
    it "Rewriting functions using folds - 1 - myOr (6)" $ do (myOr infiniteTrue) == True

    it "Rewriting functions using folds - 2 - myAny (1)" $ do (myAny even [1,3,5]) == False
    it "Rewriting functions using folds - 2 - myAny (2)" $ do (myAny odd [1,3,5]) == True
    it "Rewriting functions using folds - 2 - myAny (3)" $ do (myAny even [2,2]) == True
    it "Rewriting functions using folds - 2 - myAny (4)" $ do (myAny even [1,2]) == True
    it "Rewriting functions using folds - 2 - myAny (5)" $ do (myAny even [2,1]) == True
    it "Rewriting functions using folds - 2 - myAny (6)" $ do (myAny even [1,1]) == False
    it "Rewriting functions using folds - 2 - myAny (7)" $ do (myAny even [1,2,1]) == True
    it "Rewriting functions using folds - 2 - myAny (8)" $ do (myAny odd [2,1,2]) == True

    it "Rewriting functions using folds - 3 - myElement (1)" $ do (myElem 1 [1..10]) == True
    it "Rewriting functions using folds - 3 - myElement (2)" $ do (myElem 5 [1..10]) == True
    it "Rewriting functions using folds - 3 - myElement (3)" $ do (myElem 1 [2..10]) == False

    it "Rewriting functions using folds - 4 - reverse (1)" $ do (myReverse "blah") == "halb"
    it "Rewriting functions using folds - 4 - reverse (2)" $ do (myReverse [1..3]) == [3,2,1]

    it "Rewriting functions using folds - 5 - myMap (1)" $ do (myMap odd [1..3]) == [True, False, True]
    it "Rewriting functions using folds - 5 - myMap (2)" $ do (myMap even [1..3]) == [False, True, False]

    it "Rewriting functions using folds - 6 - myFilter (1)" $ do (myFilter odd [1..3]) == [1,3]
    it "Rewriting functions using folds - 6 - myFilter (2)" $ do (myFilter even [1..3]) == [2]

    it "Rewriting functions using folds - 7 - squish (1)" $ do (squish [[1, 2], [3, 4]]) == [1, 2, 3, 4]

    it "Rewriting functions using folds - 8 - squishMap (1)" $ do (squishMap (\x -> [1, x, 3]) [2]) == [1, 2, 3]
    it "Rewriting functions using folds - 8 - squishMap (2)" $ do (squishMap (\x -> "WO " ++ [x] ++ " OT ") "blah") == "WO b OT WO l OT WO a OT WO h OT "

    it "Rewriting functions using folds - 9 - squishAgain (1)" $ do (squishAgain [[1, 2], [3, 4]]) == [1, 2, 3, 4]

    it "Rewriting functions using folds - 10 - myMaximumBy (1)" $ do (myMaximumBy compare [1, 53, 9001, 10]) == 9001
    it "Rewriting functions using folds - 10 - myMaximumBy (2)" $ do (myMaximumBy compare ['a','b']) == 'b'
    it "Rewriting functions using folds - 10 - myMaximumBy (4)" $ do (myMaximumBy compare ['a','b','a']) == 'b'
    it "Rewriting functions using folds - 10 - myMaximumBy (4)" $ do (myMaximumBy compare [1..10]) == 10
    it "Rewriting functions using folds - 10 - myMaximumBy (5)" $ do (myMaximumBy (\_ _ -> GT) [1..10]) == 1
    it "Rewriting functions using folds - 10 - myMaximumBy (6)" $ do (myMaximumBy (\_ _ -> LT) [1..10]) == 10

    it "Rewriting functions using folds - 11 - myMinimumBy (1)" $ do (myMinimumBy compare [1, 53, 9001, 10]) == 1
    it "Rewriting functions using folds - 11 - myMinimumBy (2)" $ do (myMinimumBy compare ['a','b']) == 'a'
    it "Rewriting functions using folds - 11 - myMinimumBy (4)" $ do (myMinimumBy compare ['a','b','a']) == 'a'
    it "Rewriting functions using folds - 11 - myMinimumBy (4)" $ do (myMinimumBy compare [1..10]) == 1
    it "Rewriting functions using folds - 11 - myMinimumBy (5)" $ do (myMinimumBy (\_ _ -> GT) [1..10]) == 10
    it "Rewriting functions using folds - 11 - myMinimumBy (6)" $ do (myMinimumBy (\_ _ -> LT) [1..10]) == 1

