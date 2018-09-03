{-# LANGUAGE FlexibleInstances #-}
module Chapter11Spec(spec) where
import Test.Hspec
import Data.Int
import Data.Char
import Data.List
data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini| Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)
----------------------------------------
myCar :: Vehicle
myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir
------------------------------------
isCar :: Vehicle -> Bool
isCar v = case v of
             Car _ _  -> True
             _        ->  False
-------------------------------------
isPlane :: Vehicle -> Bool
isPlane v = case v of
             Plane _  -> True
             _        ->  False
-------------------------------------
areCars :: [Vehicle] -> [Bool]
areCars b = map isCar b
-------------------------------------
getManu :: Vehicle -> Manufacturer
getManu v = case v of
             Car m _  -> m
             _        ->  undefined
--------------------------------------
class TooMany a where tooMany :: a -> Bool

instance TooMany (Int, String) where tooMany (n, _) = n > 42
instance TooMany (Int, Int) where tooMany (i, i') = i + i' > 42

------------------------------------------

data OperatingSystem =
    GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgrammingLanguage =
    Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
    , lang :: ProgrammingLanguage }
    deriving (Eq, Show)


allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer {os=x,lang=y} | x <- allOperatingSystems, y<-allLanguages]

------------------------------------------
data BinaryTree a =Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)
-------------------------------------------
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ (preorder left) ++ (preorder right)
-------------------------------------------
inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)
-------------------------------------------
postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]
-------------------------------------------

ciphers :: [Char] -> [Char] -> [Char]
ciphers [] _ = []
ciphers l key =  go l key 0
    where
        go :: [Char] -> [Char] -> Int -> [Char]
        go [] _ _ = []
        go (x:xs) key count = chr (ord x + ((ord (key !! (mod count (length key)))) - ord 'A')) : go xs key (count+1)

unCiphers :: [Char] -> [Char] -> [Char]
unCiphers [] _ = []
unCiphers l key =  go l key 0
    where
        go :: [Char] -> [Char] -> Int -> [Char]
        go [] _ _ = []
        go (x:xs) key count = chr (ord x - ((ord (key !! (mod count (length key)))) - ord 'A')) : go xs key (count+1)

-------------------------------------------
--phone exercise
type Digit = Char
type Presses = Int
reverseMap :: Digit -> [(Digit, Presses)]
reverseMap '1' = [('1', 1)]  
reverseMap '.' = [('#' , 1)]
reverseMap ',' = [('#' , 2)]

reverseMap ' ' = [('0' , 1)]
reverseMap '+' = [('0' , 2)]
reverseMap '_' = [('0' , 3)]
reverseMap '0' = [('0' , 4)]

reverseMap 'a' = [('2' , 1)]
reverseMap 'b' = [('2' , 2)]
reverseMap 'c' = [('2' , 3)]
reverseMap '2' = [('2' , 4)]

reverseMap 'd' = [('3' , 1)]
reverseMap 'e' = [('3' , 2)]
reverseMap 'f' = [('3' , 3)]
reverseMap '3' = [('3' , 4)]

reverseMap 'g' = [('4' , 1)]
reverseMap 'h' = [('4' , 2)]
reverseMap 'i' = [('4' , 3)]
reverseMap '4' = [('4' , 4)]

reverseMap 'j' = [('5' , 1)]
reverseMap 'k' = [('5' , 2)]
reverseMap 'l' = [('5' , 3)]
reverseMap '5' = [('5' , 4)]

reverseMap 'm' = [('6' , 1)]
reverseMap 'n' = [('6' , 2)]
reverseMap 'o' = [('6' , 3)]
reverseMap '6' = [('6' , 4)]

reverseMap 'p' = [('7' , 1)]
reverseMap 'q' = [('7' , 2)]
reverseMap 'r' = [('7' , 3)]
reverseMap 's' = [('7' , 4)]
reverseMap '7' = [('7' , 5)]

reverseMap 't' = [('8' , 1)]
reverseMap 'u' = [('8' , 2)]
reverseMap 'v' = [('8' , 3)]
reverseMap '8' = [('8' , 4)]

reverseMap 'w' = [('9' , 1)]
reverseMap 'x' = [('9' , 2)]
reverseMap 'y' = [('9' , 3)]
reverseMap 'z' = [('9' , 4)]
reverseMap '4' = [('9' , 5)]

reverseMap 'A' = [('*' , 1), ('2' , 1)]
reverseMap 'B' = [('*' , 1), ('2' , 2)]
reverseMap 'C' = [('*' , 1), ('2' , 3)]

reverseMap 'D' = [('*' , 1), ('3' , 1)]
reverseMap 'E' = [('*' , 1), ('3' , 2)]
reverseMap 'F' = [('*' , 1), ('3' , 3)]

reverseMap 'G' = [('*' , 1), ('4' , 1)]
reverseMap 'H' = [('*' , 1), ('4' , 2)]
reverseMap 'I' = [('*' , 1), ('4' , 3)]

reverseMap 'J' = [('*' , 1), ('5' , 1)]
reverseMap 'K' = [('*' , 1), ('5' , 2)]
reverseMap 'L' = [('*' , 1), ('5' , 3)]

reverseMap 'M' = [('*' , 1), ('6' , 1)]
reverseMap 'N' = [('*' , 1), ('6' , 2)]
reverseMap 'O' = [('*' , 1), ('6' , 3)]

reverseMap 'P' = [('*' , 1), ('7' , 1)]
reverseMap 'Q' = [('*' , 1), ('7' , 2)]
reverseMap 'R' = [('*' , 1), ('7' , 3)]
reverseMap 'S' = [('*' , 1), ('7' , 4)]

reverseMap 'T' = [('*' , 1), ('8' , 1)]
reverseMap 'U' = [('*' , 1), ('8' , 2)]
reverseMap 'V' = [('*' , 1), ('8' , 3)]

reverseMap 'W' = [('*' , 1), ('9' , 1)]
reverseMap 'X' = [('*' , 1), ('9' , 2)]
reverseMap 'Y' = [('*' , 1), ('9' , 3)]
reverseMap 'Z' = [('*' , 1), ('9' , 4)]

convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]

reverseTaps :: Char -> [(Digit, Presses)]
reverseTaps char = reverseMap char

cellPhonesDead :: String -> [(Digit, Presses)]
cellPhonesDead string = foldl (\b a  -> b ++ reverseTaps a) []  string

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps l = foldl (\b a  -> b + snd(a)) 0 l


sortTuple (a1,b1) (a2,b2)
    | b1 > b2 = GT
    | b1 < b2 = LT
    | b1 == b2 = EQ

mostPopularLetter :: [Char] -> (Char, Int)
mostPopularLetter string = reverse (sortBy sortTuple ( map (\x -> (x, go string x 0)) string)) !! 0
     where
        go :: [Char] -> Char -> Int -> Int
        go string char count = case string of
            [] -> count
            x : xs -> if (x == char) then go xs char (count + 1) else go xs char count

mostPopularWord :: [Char] -> [Char]
mostPopularWord string = fst $ reverse (sortBy sortTuple (map (\x -> (x, go ws x 0)) ws)) !! 0
     where
        ws = words string
        go :: [[Char]] -> String -> Int -> Int
        go string s count = case string of
            [] -> count
            x : xs -> if (x == s) then go xs s count + 1 else go xs s count
---------------------------------
--Hutton’s Razor

data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval expr = case expr of
     Add (Lit a) (Lit b) -> a + b

printExpr :: Expr -> String
printExpr expr = case expr of
    Lit a -> show a
    Add a b -> printExpr a ++ " + " ++ printExpr b


---------------------------------
spec :: Spec
spec = do

  describe "Chapter 11" $ do
    it "Exercises: Vehicles 2 - isCar (1)" $ do (isCar myCar) == True

    it "Exercises: Vehicles 2 - isCar (2)" $ do (isCar doge) == False

    it "Exercises: Vehicles 2 - isPlane (1)" $ do (isPlane myCar) == False
    it "Exercises: Vehicles 2 - isPlane (2)" $ do (isPlane doge) == True

    it "Exercises: Vehicles 2 - areCars (1)" $ do (areCars [myCar, urCar]) == [True, True]
    it "Exercises: Vehicles 2 - areCars (2)" $ do (areCars [clownCar, doge]) == [True, False]

    it "Exercises: Vehicles 3 - getManu (1)" $ do (getManu myCar) == Mini

    it "Logic Goats 1 - (1)" $ do (tooMany (1 :: Int,"")) == False
    it "Logic Goats 1 - (2)" $ do (tooMany (100 :: Int,"")) == True

    it "Logic Goats 2 (1)" $ do (tooMany (1:: Int,2:: Int)) == False
    it "Logic Goats 2 (2)" $ do (tooMany (20:: Int,30 :: Int)) == True

    it "Programmers" $ do (length allProgrammers)  == 16

    it "BinaryTree" $ do (mapTree (+1) (Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)))  == (Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf))

    it "BinaryTree preorder" $ do (preorder (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)))  == [2,1,3]

    it "BinaryTree inorder" $ do (inorder (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)))  == [1,2,3]

    it "BinaryTree postorder" $ do (postorder (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)))  == [1,3,2]

    it "Chipers" $ do (unCiphers ( ciphers "MEET AT DAWN" "ALLY" ) "ALLY") == "MEET AT DAWN"

    it "reverseTaps 1" $ do (reverseTaps 'a') == [('2' ,1)]
    it "reverseTaps 2" $ do (reverseTaps 'A') == [('*' ,1), ('2' ,1)]
    it "cellPhonesDead 1" $ do (cellPhonesDead "aa") == [('2' ,1), ('2' ,1)]
    it "cellPhonesDead 2" $ do (cellPhonesDead "aA") == [('2' ,1),('*' ,1),  ('2' ,1)]
    it "fingerTaps" $ do (fingerTaps $ cellPhonesDead "Wanna play 20 questions") == 50

    --   question 3. How many times do digits need to be pressed for each message?
    it "convo 1" $ do (map (\a -> (a, fingerTaps(cellPhonesDead a))) convo ) ==[("Wanna play 20 questions", 50), ("Ya", 5), ("U 1st haha", 17),
                                                                             ("Lol ok. Have u ever tasted alcohol lol", 81), ("Lol ya", 15), ("Wow ur cool haha. Ur turn", 49),
                                                                             ("Ok. Do u think I am pretty Lol", 58), ("Lol ya", 15), ("Haha thanks just making sure rofl ur turn", 80)]
    it "fingerTaps" $ do (fingerTaps [('a' , 1), ('b' , 2), ('c' , 1)]) == 4

    --    question 4 What was the most popular letter for each message? What was its cost?
    it "convo 2" $ do (map (\a -> (a, mostPopularLetter a)) convo ) ==[("Wanna play 20 questions", ('n', 3)), ("Ya", ('a', 1)), ("U 1st haha", ('a', 2)),
                                                                              ("Lol ok. Have u ever tasted alcohol lol", (' ', 7)), ("Lol ya", ('a', 1)), ("Wow ur cool haha. Ur turn", (' ', 5)),
                                                                              ("Ok. Do u think I am pretty Lol", (' ', 7)), ("Lol ya", ('a', 1)), ("Haha thanks just making sure rofl ur turn", (' ', 7))]

    --question 5 What was the most popular letter overall? What was the most popular word?

    it "mostPopularLetter 1" $ do (mostPopularLetter ( foldl (++) [] convo)) == (' ',33)
    it "mostPopularWord 1" $ do (mostPopularWord ( foldl (\x y -> x ++ " " ++y) [] convo)) == "Lol"
    it "mostPopularLetter 1" $ do (fst $ mostPopularLetter "Aaa") == 'a'
    it "mostPopularLetter 1" $ do (fst $ mostPopularLetter "acaba") == 'a'
    it "mostPopularLetter 1" $ do (fst $ mostPopularLetter "aaA") == 'a'
    it "mostPopularLetter 1" $ do (fst $ mostPopularLetter "AaA") == 'A'
    it "mostPopularWord 2" $ do (mostPopularWord "aaa bbb aaa") == "aaa"

    it "Hutton’s Razor eval" $ do (eval $ Add (Lit 1) (Lit 9001)) == 9002
    it "Hutton’s Razor printExpr 1" $ do (printExpr $ Add (Lit 1) (Lit 9001)) == "1 + 9001"
    it "Hutton’s Razor printExpr 2" $ do
                                    let a1 = Add (Lit 9001) (Lit 1)
                                    let a2 = Add a1 (Lit 20001)
                                    let a3 = Add (Lit 1) a2
                                    printExpr a3 == "1 + 9001 + 1 + 20001"
