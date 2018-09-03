module Chapter12Spec(spec) where
import Test.Hspec
import Data.Either

notThe :: String -> Maybe String
notThe string = case (string == "the") of
    True -> Nothing
    False -> Just string

---------------------------------
replaceThe :: String -> String
replaceThe string = go $ words string
    where
    go :: [String] -> String
    go string = case string of
        [] -> ""
        x:[] -> if ((notThe x) == Nothing) then "a" else x
        x:xs -> if ((notThe x) == Nothing) then "a" ++ " " ++ go xs else x ++ " " ++ go xs

---------------------------------

vowels = "aeiou"
isVowel :: Char -> Bool
isVowel c = elem c vowels
--
startWithVowel :: String -> Bool
startWithVowel string = isVowel $ string !! 0

--

countTheBeforeVowel :: String -> Int
countTheBeforeVowel s = go $ words s
    where
        go :: [String] -> Int
        go s = case s of
            [] -> 0
            _:[] -> 0
            the : (x : xs) -> if (((notThe the) == Nothing) && (startWithVowel x)) then (1 + go xs) else go (x:xs)
---------------------------------

count:: String -> Int
count s = length s
--
vowelsOfString :: [Char] -> [Char]
vowelsOfString s = case s of
    [] -> ""
    x : xs -> if (isVowel x) then (x : (vowelsOfString xs)) else (vowelsOfString xs)
--
countVowels :: String -> Int
countVowels string = count ( vowelsOfString string)
---------------------------------
newtype Word' = Word' String deriving (Eq, Show)
mkWord :: String -> Maybe Word'
mkWord string = if (countVowels string < count string - countVowels string ) then Just (Word' string) else Nothing

---------------------------------
data Nat = Zero | Succ Nat deriving (Eq, Show)
natToInteger :: Nat -> Integer
natToInteger nat = case nat of
    Zero -> 0
    Succ n -> 1 + natToInteger n

--
integerToNat :: Int -> Maybe Nat
integerToNat int = if (int < 0) then Nothing else Just $ go int
    where
        go :: Int -> Nat
        go int = case int of
            0 -> Zero
            1 -> Succ Zero
            n -> Succ $ go (n -1)
---------------------------------
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just b) = True
---------------------------------
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee b f (Just a) = f a

---
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False
--
fromMaybe :: a -> Maybe a -> a
fromMaybe b Nothing = b
fromMaybe b (Just a) = a
--
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x
--

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]
--
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = case x of
    Just(x1) -> x1 : catMaybes xs
    otherwise -> catMaybes xs
--
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe a = case (go a []) of
    [] -> Nothing
    a -> Just a
    where
         go :: [Maybe a] -> [a] -> [a]
         go [] acc = acc
         go (Nothing:_) _ = []
         go ((Just x):xs) acc = go xs (acc ++ [x])

---------------------------------
lefts' :: [Either a b] -> [a]
lefts' xs = foldr go [] xs
  where
    go (Left x) xs = x : xs
    go _ xs = xs

---------------------------------
--rights' :: [Either a b] -> [a]
rights' [] = []
rights' ((Right x):[]) = [x]
rights' ((Right x):xs) = x:(rights' xs)
rights' ((Left _):[]) = []
rights' ((Left _):xs) = rights' xs

---------------------------------
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' a = (lefts' a,rights' a)

---------------------------------
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f a = case a of
    (Left _) -> Nothing
    (Right x) -> Just(f x)

---------------------------------
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f1 f2 x = case x of
    (Left x) -> f1 x
    (Right x) -> f2 x

---------------------------------
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f2 (Right x) = Just $ either' undefined f2 (Right x)
eitherMaybe'' _ _ = Nothing
-------
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a:myIterate f (f a)

-------
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case (f b) of
    (Just z) -> (fst z) : myUnfoldr f (snd z)
    otherwise -> []
-------
betterIterate :: (a -> a) -> a -> [a]
betterIterate f a = myUnfoldr (\x -> Just (x,(f x))) a
--------------------
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = case (f a) of
    Nothing -> Leaf
    Just (z1,z2,z3) -> Node (unfold f z1) z2 (unfold f z3)

---------------

spec :: Spec
spec = do

  describe "Chapter 12" $ do
    it "String processing 1 (1)" $ do (notThe "the") == Nothing
    it "String processing 1 (2)" $ do (notThe "blahtheblah") == Just "blahtheblah"
    it "String processing 1 (3)" $ do (notThe "woot") == Just "woot"

    it "the cow loves us" $ do (replaceThe "the cow loves us") == "a cow loves us"
    it "the cow loves us" $ do (replaceThe "cow loves us") == "cow loves us"

    it "String processing 2 (1)" $ do (countTheBeforeVowel "the cow") == 0
    it "String processing 2 (2)" $ do (countTheBeforeVowel "the evil cow") == 1
    it "String processing 2 (3)" $ do (countTheBeforeVowel "the evil cow the uncle") == 2

    it "String processing 3 (1)" $ do ( vowelsOfString "the cow") == "eo"
    it "String processing 3 (2)" $ do (count $ vowelsOfString "the cow") == 2
    it "String processing 3 (3)" $ do ( vowelsOfString "Mikolajczak") == "ioaa"
    it "String processing 3 (4)" $ do ( count $ vowelsOfString "Mikolajczak") == 4

    it "Validate the word (1)" $ do ( mkWord ("aaav")) == Nothing
    it "Validate the word (2)" $ do ( mkWord ("avvv")) == Just (Word' "avvv")

    it "It’s only Natural natToInteger (1)" $ do ( natToInteger Zero) == 0
    it "It’s only Natural natToInteger (2)" $ do ( natToInteger $ Succ Zero) == 1
    it "It’s only Natural natToInteger (3)" $ do ( natToInteger $ Succ (Succ Zero)) == 2

    it "It’s only Natural integerToNat (1)" $ do ( integerToNat 0) == Just Zero
    it "It’s only Natural integerToNat (2)" $ do ( integerToNat 1) == Just (Succ Zero)
    it "It’s only Natural integerToNat (3)" $ do ( integerToNat 2) == Just ( Succ (Succ Zero))
    it "It’s only Natural integerToNat (4)" $ do ( integerToNat (-1)) == Nothing

    it "Small library for Maybe 1 (1)" $ do ( isJust (Just 1)) == True
    it "Small library for Maybe 1 (2)" $ do ( isJust (Just (Just 1))) == True
    it "Small library for Maybe 1 (3)" $ do ( isJust (Nothing)) == False

    it "Small library for Maybe 2 (1)" $ do ( mayybee 0 (\a -> a + 1) (Just 1)) == 2
    it "Small library for Maybe 2 (2)" $ do ( mayybee 0 (\a -> a + 1) Nothing) == 0
    it "Small library for Maybe 2 (3)" $ do ( isNothing (Just 1)) == False
    it "Small library for Maybe 2 (4)" $ do ( isNothing Nothing) == True

    it "Small library for Maybe 3 (1)" $ do ( fromMaybe 0 Nothing) == 0
    it "Small library for Maybe 3 (2)" $ do ( fromMaybe 0 (Just 1)) == 1

    it "Small library for Maybe 4 (1)" $ do ( listToMaybe [1,2,3]) == Just 1
    it "Small library for Maybe 4 (2)" $ do ( listToMaybe [] ) == (Nothing :: Maybe Int)

    it "Small library for Maybe 4 (3)" $ do ( maybeToList (Just 1) ) == [1]
    it "Small library for Maybe 4 (4)" $ do ( maybeToList Nothing ) == ([] :: [Int])

    it "Small library for Maybe 5 (1)" $ do ( catMaybes [Just 1,Nothing,Just 2]) == [1,2]

    it "Small library for Maybe 6 (1)" $ do ( flipMaybe [ Just 1,Just 2,Just 3]) == Just [1,2,3]
    it "Small library for Maybe 6 (2)" $ do ( flipMaybe [ Just 1,Nothing,Just 2]) == Nothing

    it "Small library for Either 1 (1)" $ do (lefts' [Left 1,Left 2, Right "a"]) == [1,2]

    it "Small library for Either 2 (1)" $ do (rights' [Left 1,Left 2, Right "a"]) == ["a"]
    it "Small library for Either 2 (2)" $ do (rights' [Left 1,Left 2, Right "a",Left 3]) == ["a"]
    it "Small library for Either 2 (3)" $ do (rights' [Left 1,Left 2]::[Either Int Int]) == []
    it "Small library for Either 2 (4)" $ do (rights' [Right "a", Left 1, Left 2, Right "b"]) == ["a", "b"]
    it "Small library for Either 2 (5)" $ do (rights' [Right "a"]) == ["a"]

    it "Small library for Either 3 (1)" $ do (partitionEithers' [Left 1, Left 2, Right "a"]) == ([1,2],["a"])
    it "Small library for Either 3 (2)" $ do (partitionEithers' [Left 1, Left 2, Right "a", Left 3]) == ([1,2,3],["a"])
--    it "Small library for Either 3 (3)" $ do (partitionEithers' [Left 1, Left 2]) == ([1,2],[]))
    it "Small library for Either 3 (4)" $ do (partitionEithers' [Right "a", Left 1, Left 2, Right "b"]) == ([1,2],["a","b"])
--    it "Small library for Either 3 (5)" $ do (partitionEithers' [Right "a"]) == ([],["a"])

    it "Small library for Either 4 (1)" $ do (eitherMaybe' (\a -> a ++ " -> ok") (Right "1")) == (Just "1 -> ok")
    it "Small library for Either 4 (2)" $ do (eitherMaybe' (\a -> a) (Left 1)) == (Nothing::(Maybe Int))

--    it "Small library for Either 5 (1)" $ do (either' (\a -> a ++ " -> ko") (\a -> a ++ " - ok") (Right 1)) == "1 -> ok"
--    it "Small library for Either 5 (2)" $ do (either' (\a -> a ++ " -> ko") (\a -> a ++ " - ok") (Left 2)) == "2 -> ko"

--    it "Small library for Either 6 (1)" $ do (eitherMaybe'' (\a -> a ++ " -> ok") (Right 1)) == Just ("1 -> ok")
    it "Small library for Either 6 (2)" $ do (eitherMaybe'' (\a -> a ++ " -> ok") (Left 1)) == Nothing

    it "Write your own iterate and unfoldr 1 (1)" $ do (take 4 $ myIterate (\a -> a + 1) 0) == [0,1,2,3]
    it "Write your own iterate and unfoldr 1 (2)" $ do (take 4 $ myUnfoldr (\a -> (Just (a,a + 1)) ) 0) == [0,1,2,3]
    it "Write your own iterate and unfoldr 1 (3)" $ do (take 4 $ betterIterate (\a -> a + 1) 0) == [0,1,2,3]

--    it "Finally something other than a list!" $ do (take 3 $ unfold (\a -> Just((a + 1, a, a + 1))) 0) == Node (Node Leaf 1 Leaf) 0 (Node Leaf 1 Leaf)TODO

