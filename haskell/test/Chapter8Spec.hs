module Chapter8Spec(spec) where
import Test.Hspec
import Data.List (intersperse)

--recursion

sums :: (Eq a, Num a) => a -> a
sums n = go n
    where go count
            | count == 0 = 0
            | otherwise = count + go (count - 1)

------ multiply by sum

mul :: (Eq a, Num a) => a -> a -> a
mul n m = go (n, m)
    where go (x, count)
            | count == 0 = 0
            | otherwise = x + go (x, count - 1)

--Fixing dividedBy
data DividedResult =  Result Integer | DividedByZero deriving Eq
dividedBy :: Integer -> Integer -> (DividedResult, Integer)
dividedBy num denom = go num denom 0
  where go n d count
          | d == 0 = (DividedByZero,0)
          | n < 0  = go (negate n) d count
          | d < 0  = go n (negate d) count
          | n < d = (Result( count * if num<0 && denom >0|| denom <0 && num >0 then -1 else 1), n)
          | otherwise = go (n - d) d (count + 1)

--McCarthy 91 function
mcCarthy91 :: Integer -> Integer
mcCarthy91 n = go n
    where go n
            | n > 100 = n - 10
            | otherwise = go . go $ n + 11
--Numbers into words

digitToWord :: Int -> String
digitToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"

-- 123 -> [1,2,3]
digits :: Int -> [Int]
digits n = go n []
    where go n acc
            | n < 10 = n : acc
            | otherwise =  go ( div n 10)( mod n 10 : acc)

-- 123 -> " one two three"
wordNumber :: Int -> String
wordNumber n =concat (map (\n -> " " ++ n) ( map digitToWord (digits n)))

spec :: Spec
spec = do


  describe "Chapter 8" $ do
  
    it "2. recursion - sums" $ do sums 5 == 15

  it "3. multiply by sum" $ do mul 5 3 == 15

  it "Fixing dividedBy 1" $ do dividedBy 6 0 == (DividedByZero,0)

  it "Fixing dividedBy 2" $ do dividedBy 6 2 == (Result(3),0)

  it "Fixing dividedBy 3" $ do dividedBy 6 (-2) == (Result(-3),0)

  it "Fixing dividedBy 4" $ do dividedBy (-6) 2 == (Result(-3),0)

  it "Fixing dividedBy 5" $ do dividedBy (-7) 2 == (Result(-3),1)

  it "McCarthy 1" $ do mcCarthy91 1 == 91

  it "McCarthy 2" $ do mcCarthy91 200 == 190

  it "digits 123" $ do digits 123 == [1,2,3]

  it "wordNumber 123" $ do wordNumber 123 == " one two three"