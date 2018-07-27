module Chapter9Spec(spec) where
import Test.Hspec
import Data.Bool
import Data.Char

--enumFromTo
eftInt :: Int -> Int -> [Int]
eftInt start stop = go stop start []
  where go from to acc
         | from == to  = from : acc
         | otherwise   = go  (from -1)  to (from : acc)
--------------
eftChar :: Char -> Char -> [Char]
eftChar start stop = go stop start []
  where go from to acc
         | from == to  = from : acc
         | otherwise   = go  (pred from )  to (from : acc)
--------------
eftBool :: Bool -> Bool -> [Bool]
eftBool True False = [True, False]
eftBool True True = [True]
eftBool False False = [False]
eftBool a b = [a , b]

--------------
eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd EQ EQ = [EQ]
eftOrd _ LT = []
eftOrd start stop = go stop start []
  where go from to acc
         | from == to  = EQ : acc
         | otherwise   = go (pred from )  to (from : acc)

--------------
myWords :: String -> [String]
myWords s = go s []
  where go s acc
         | s == ""      = acc
         | otherwise    =  go (dropWhile( == ' ') (dropWhile (/= ' ') s)) (acc ++ [(takeWhile (/= ' ') s )])

--------------
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines s = go s []
  where go s acc
         | s == ""      = acc
         | otherwise    =  go (dropWhile( == '\n') (dropWhile (/= '\n') s)) (acc ++ [(takeWhile (/= '\n') s )])

--------------
myWordsANDmyLines :: String -> Char -> [String]
myWordsANDmyLines s sep = go s sep []
        where go s sep acc
               | s == ""      = acc
               | otherwise    =  go (dropWhile( == sep) (dropWhile (/= sep) s)) sep (acc ++ [(takeWhile (/= sep) s )])

--------------
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

squareCube1 = [(x,y) | x <- mySqr, y<- myCube]

squareCube2 = [(x,y) | x <- mySqr, y<- myCube,  x < 50 && y < 50]
---------------
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys
---------------
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
---------------
chapterExercises4 :: [Char] -> [Char]
chapterExercises4 [] = []
chapterExercises4 (x:xs) = (toUpper $ x) : chapterExercises4 (xs)
---------------

ciphers :: [Char] -> [Char]
ciphers [] = []
ciphers (x:xs) =  chr (ord x + ord 'a') : ciphers xs

unCiphers :: [Char] -> [Char]
unCiphers [] = []
unCiphers (x:xs) =  chr (ord x - ord 'a') : unCiphers xs
---------------
squish :: [[a]] -> [a]
squish [] = []
squish [x] = x
squish (x : xs) = x ++ squish xs
---------------
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x : xs) = f x ++ squishMap f xs
---------------
squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain [x] = x
squishAgain (x : xs) = x ++ squishMap (\x -> x) xs
-----------------

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:xs) = goMax f xs x
    where
        goMax :: (a -> a -> Ordering) -> [a] -> a -> a
        goMax _ [] m = m
        goMax f (x : xs) m = goMax f xs (if (f x m == GT) then x else m)
-----------------
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ (x:[]) = x
myMinimumBy f (x:xs) = goMin f xs x
    where
        goMin :: (a -> a -> Ordering) -> [a] -> a -> a
        goMin _ [] m = m
        goMin f (x : xs) m = goMin f xs (if (f x m == LT) then x else m)

-------------------
myMinimum ::  Ord a => [a] -> a
myMinimum = myMinimumBy compare
-------------------
myMaximum ::  Ord a => [a] -> a
myMaximum = myMaximumBy compare
--------------------------

spec :: Spec
spec = do

  describe "Chapter 9" $ do
  
    it "eftInt" $ do eftInt 10 12 == [10,11,12]

    it "eftChar" $ do eftChar 'a' 'c' == "abc"

    it "eftBool 1" $ do eftBool True False == [True, False]

    it "eftBool 2" $ do eftBool False True == [False,True]

    it "eftBool 3" $ do eftBool False False == [False]

    it "eftBool 4" $ do eftBool True True == [True]

    it "eftOrd 1" $ do eftOrd EQ EQ == [EQ]

    it "eftOrd 2" $ do eftOrd EQ LT == []

  it "Thy Fearful Symmetry" $ do myWords "all i wanna do is have some fun"  == ["all", "i", "wanna", "do", "is", "have", "some", "fun"]

  it "PoemLines" $ do myLines sentences  == [ "Tyger Tyger, burning bright", "In the forests of the night", "What immortal hand or eye", "Could frame thy fearful symmetry?"]

  it "Thy Fearful Symmetry" $ do myWordsANDmyLines "all i wanna do is have some fun" ' ' == ["all", "i", "wanna", "do", "is", "have", "some", "fun"]

  it "PoemLines" $ do myWordsANDmyLines sentences '\n'  == [ "Tyger Tyger, burning bright", "In the forests of the night", "What immortal hand or eye", "Could frame thy fearful symmetry?"]

  it "squareCube1" $ do squareCube1 == [(1.0, 1.0), (1.0, 8.0), (1.0, 27.0), (1.0, 64.0), (1.0, 125.0), (4.0, 1.0), (4.0, 8.0), (4.0, 27.0), (4.0, 64.0), (4.0, 125.0), (9.0, 1.0), (9.0, 8.0), (9.0, 27.0), (9.0, 64.0), (9.0, 125.0), (16.0, 1.0), (16.0, 8.0), (16.0, 27.0), (16.0, 64.0), (16.0, 125.0), (25.0, 1.0), (25.0, 8.0), (25.0, 27.0), (25.0, 64.0), (25.0, 125.0)]

  it "squareCube2" $ do squareCube2 == [(1.0, 1.0), (1.0, 8.0), (1.0, 27.0), (4.0, 1.0), (4.0, 8.0), (4.0, 27.0), (9.0, 1.0), (9.0, 8.0), (9.0, 27.0), (16.0, 1.0), (16.0, 8.0), (16.0, 27.0), (25.0, 1.0), (25.0, 8.0), (25.0, 27.0)]

  it "squareCube3" $ do length squareCube2 == 15

  it "foldBool list" $ do map (\x -> bool 'a' 'b' x) [True, False] == ['b','a']

  it "Filtering 1" $ do filter (\x -> mod x 3 == 0) [1..30] == [3, 6, 9, 12, 15, 18, 21, 24, 27, 30]

  it "Filtering 2" $ do (length $ filter (\x -> mod x 3 == 0) [1..30]) == 10

  it "Filtering 3" $ do filter( \x -> not (elem x ["the", "a", "an"])) (words "the brown dog was a goof") == ["brown", "dog", "was", "goof"]

  it "Zipping exercises 1" $ do myZip [1,2,3] ['a','b'] == [(1,'a'), (2,'b')]

  it "Zipping exercises 2" $ do myZipWith (\x y -> x + y) [1,2] [1,1] == [2,3]

  it "Zipping exercises 3" $ do myZipWith (\x y -> (x , y)) [1,2,3] ['a','b'] == [(1,'a'), (2,'b')]

  it "Chapter Exercises 2" $ do filter (\x -> isUpper x == True) "HbEfLrLxO," == "HELLO"

  it "Chapter Exercises 3" $ do (toUpper $ head "julie") : (tail "julie") == "Julie"

  it "Chapter Exercises 4" $ do chapterExercises4 "woot" == "WOOT"

  it "Chapter Exercises 5" $ do (toUpper $ head "julie") == 'J'

  it "Chipers" $ do (unCiphers $ ciphers "hello") == "hello"

  it "squish" $ do squish [[1,2],[3]] == [1,2,3]

  it "squishMap 1" $ do squishMap (\x -> [1, x, 3]) [2] == [1,2,3]

  it "squishMap 2" $ do squishMap (\x -> "WO "++[x]++" HOO ") "123" == "WO 1 HOO WO 2 HOO WO 3 HOO "

  it "squishAgain" $ do squishAgain [[1,2],[3]] == [1,2,3]

  it "myMaximumBy" $ do myMaximumBy compare [1, 53, 9001, 10] == 9001

  it "myMaximumBy2 " $ do myMaximumBy compare ['a','b'] == 'b'

  it "myMinimumBy" $ do myMinimumBy compare [1, 53, 9001, 10] == 1

  it "myMinimumBy 2" $ do myMinimumBy compare ['a','b'] == 'a'

  it "myMinimum" $ do myMinimum [1, 53, 9001, 10] == 1

  it "myMinimum 2" $ do myMinimum ['a','b'] == 'a'

  it "myMaximum" $ do myMaximum [1, 53, 9001, 10] == 9001

  it "myMaximum 2" $ do myMaximum ['a','b'] == 'b'
