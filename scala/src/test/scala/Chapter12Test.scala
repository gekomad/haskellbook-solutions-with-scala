import org.scalatest.FunSuite

class Chapter12Test extends FunSuite {

  //notThe :: String -> Maybe String
  def notThe(string: String): Option[String] = string == ("the") match {
    case true => None
    case _ => Some(string)
  }

  test("String processing 1") {

    assert(notThe("the") == None)
    assert(notThe("blahtheblah") == Some("blahtheblah"))
    assert(notThe("woot") == Some("woot"))

    //replaceThe :: String -> String
    def replaceThe(string: String): String = {

      def go(s: Seq[String]): String = s match {
        case Nil => ""
        case x :: Nil => if (notThe(x) == None) "a" else x
        case x :: xs => if (notThe(x) == None) "a" + " " + go(xs) else x + " " + go(xs)

      }

      go(MyPredef.words(string))
    }

    assert(replaceThe("the cow loves us") == "a cow loves us")
    assert(replaceThe("cow loves us") == "cow loves us")
  }

  val vowels = "aeiou"

  def isVowel(c: Char): Boolean = vowels.contains(c)


  test("String processing 2") {

    def startWithVowel(string: String): Boolean = if (string.isEmpty || !isVowel(string(0))) false else true


    //countTheBeforeVowel :: String -> Integer
    def countTheBeforeVowel(string: String): Int = {

      def go(s: Seq[String]): Int = s match {
        case Nil => 0
        case _ :: Nil => 0
        case the :: x :: xs => if (notThe(the) == None && startWithVowel(x)) 1 + go(xs) else go(x :: xs)
      }

      go(MyPredef.words(string))
    }

    assert(countTheBeforeVowel("the cow") == 0)
    assert(countTheBeforeVowel("the evil cow") == 1)
    assert(countTheBeforeVowel("the evil cow the uncle") == 2)
  }

  def count(s: String): Int = s.size

  def vowelsOfString(s: Seq[Char]): String = s match {
    case Nil => ""
    case x :: xs => if (isVowel(x)) x + vowelsOfString(xs) else vowelsOfString(xs)
  }

  //countVowels :: String -> Integer
  def countVowels(string: String): Int = count(vowelsOfString(string.toList))

  test("String processing 3") {

    assert(vowelsOfString("the cow".toList) == "eo")
    assert(count(vowelsOfString("the cow".toList)) == 2)
    assert(vowelsOfString("Mikolajczak".toList) == "ioaa")
    assert(count(vowelsOfString("Mikolajczak".toList)) == 4)

  }

  test("Validate the word") {

    //newtype Word' = Word' String deriving (Eq, Show)
    final case class Word1(string: String)
    object Word1 {
      def apply(string: String): Option[Word1] = if (countVowels(string) < count(string) - countVowels(string)) Some(new Word1(string)) else None
    }

    assert(Word1("aaay") == None)
    assert(Word1("avvv") == Word1("avvv"))
  }

  test("It’s only Natural") {

    //data Nat = Zero | Succ Nat deriving (Eq, Show)

    trait Nat

    object Zero extends Nat
    final case class Succ(nat: Nat) extends Nat

    //    natToInteger :: Nat -> Integer

    def natToInteger(nat: Nat): Int = nat match {
      case Zero => 0
      case Succ(n) => 1 + natToInteger(n)
    }

    assert(natToInteger(Zero) == 0)
    assert(natToInteger(Succ(Zero)) == 1)
    assert(natToInteger(Succ(Succ(Zero))) == 2)

    //integerToNat :: Integer -> Maybe Nat
    def integerToNat(int: Int): Option[Nat] = {
      def go(int: Int): Nat = int match {
        case 0 => Zero
        case 1 => Succ(Zero)
        case n => Succ(go(n - 1))
      }

      if (int < 0) None else Some(go(int))
    }


    assert(integerToNat(0) == Some(Zero))
    assert(integerToNat(1) == Some(Succ(Zero)))
    assert(integerToNat(2) == Some(Succ(Succ(Zero))))
    assert(integerToNat(-1) == None)
  }

  test("Small library for Maybe 1") {
    //isJust :: Maybe a -> Bool
    def isJust(opt: Option[Any]): Boolean = opt match {
      case None => false
      case Some(_) => true
    }

    assert(isJust(Some(1)) == true)
    assert(isJust(Some(Some(1))) == true)
    assert(isJust(None) == false)
  }

  test("Small library for Maybe 2") {
    //mayybee :: b -> (a -> b) -> Maybe a -> b

    def mayybee[B, A](b: B, f: A => B, a: Option[A]): B = a match {
      case None => b
      case Some(a) => f(a)
    }

    assert(mayybee[Int, Int](0, a => a + 1, Some(1)) == 2)
    assert(mayybee[Int, Int](0, a => a + 1, None) == 0)

    //isNothing :: Maybe a -> Bool
    def isNothing[A](a: Option[A]): Boolean = a match {
      case None => true
      case _ => false
    }

    assert(isNothing(Some(1)) == false)
    assert(isNothing(None) == true)


  }

  test("Small library for Maybe 3") {
    //fromMaybe :: a -> Maybe a -> a

    def fromMaybe[A](b: A, a: Option[A]): A = a match {
      case None => b
      case Some(a) => a
    }

    assert(fromMaybe[Int](0, None) == 0)
    assert(fromMaybe[Int](0, Some(1)) == 1)

  }


  test("Small library for Maybe 4") {
    //listToMaybe :: [a] -> Maybe a

    def listToMaybe[A](a: List[A]): Option[A] = a match {
      case Nil => None
      case x :: _ => Some(x)
    }

    assert(listToMaybe(List(1, 2, 3)) == Some(1))
    assert(listToMaybe(List.empty[Int]) == None)

    //maybeToList :: Maybe a -> [a]

    def maybeToList[A](a: Option[A]): List[A] = a match {
      case None => List.empty[A]
      case Some(x) => List(x)
    }

    assert(maybeToList(Some(1)) == List(1))
    assert(maybeToList(None) == List.empty)

  }

  test("Small library for Maybe 5") {
    //catMaybes :: [Maybe a] -> [a]

    def catMaybes[A](a: List[Option[A]]): List[A] = a match {
      case Nil => Nil
      case x :: xs => x match {
        case Some(x1) => x1 :: catMaybes(xs)
        case _ => catMaybes(xs)
      }
    }

    assert(catMaybes(List(Some(1), None, Some(2))) == List(1, 2))
  }

  test("Small library for Maybe 6") {
    //flipMaybe :: [Maybe a] -> Maybe [a]


    def flipMaybe[A](a: List[Option[A]]): Option[List[A]] = {

      def go[A](a: List[Option[A]], acc: List[A] = List.empty[A]): List[A] = a match {
        case Nil => acc
        case None :: _ => Nil
        case Some(x) :: xs => go(xs, acc ::: List(x))
      }

      go(a) match {
        case a if a.isEmpty => None
        case a => Some(a)
      }
    }

    assert(flipMaybe(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
    assert(flipMaybe(List(Some(1), None, Some(3))) == None)
  }

  //lefts' :: [Either a b] -> [a]
  def lefts1[A, B](a: List[Either[A, B]]): List[A] = a match {
    case Nil => List.empty[A]
    case Left(x) :: Nil => List(x)
    case Left(x) :: xs => xs.foldRight(List(x)) { (a, list) =>
      a match {
        case Left(aa) => list ::: List(aa)
        case _ => list
      }
    }

    case Right(_) :: xs => lefts1(xs)
  }

  test("Small library for Either 1") {

    assert(lefts1[Int, String](List(Left(1), Left(2), Right("a"))) == List(1, 2))
    assert(lefts1[Int, String](List(Left(1), Left(2), Right("a"), Left(3))) == List(1, 3, 2))
    assert(lefts1[Int, String](List(Left(1), Left(2))) == List(1, 2))
    assert(lefts1[Int, String](List(Right("a"), Left(1), Left(2), Right("b"))) == List(1, 2))
    assert(lefts1[Int, String](List(Right("a"))) == List())
  }

  //rights' :: [Either a b] -> [a]
  def rights1[A, B](a: List[Either[A, B]]): List[B] = a match {
    case Nil => List.empty[B]
    case Right(x) :: Nil => List(x)
    case Right(x) :: xs => x :: rights1(xs)
    case Left(_) :: Nil => List.empty[B]
    case Left(_) :: xs => rights1(xs)
  }

  test("Small library for Either 2") {


    assert(rights1[Int, String](List(Left(1), Left(2), Right("a"))) == List("a"))
    assert(rights1[Int, String](List(Left(1), Left(2), Right("a"), Left(3))) == List("a"))
    assert(rights1[Int, String](List(Left(1), Left(2))) == List())
    assert(rights1[Int, String](List(Right("a"), Left(1), Left(2), Right("b"))) == List("a", "b"))
    assert(rights1[Int, String](List(Right("a"))) == List("a"))


  }

  test("Small library for Either 3") {

    //partitionEithers' :: [Either a b] -> ([a], [b])
    def partitionEithers1[A, B](a: List[Either[A, B]]): (List[A], List[B]) = (lefts1(a), rights1(a))

    assert(partitionEithers1[Int, String](List(Left(1), Left(2), Right("a"))) == (List(1, 2), List("a")))
    assert(partitionEithers1[Int, String](List(Left(1), Left(2), Right("a"), Left(3))) == (List(1, 3, 2), List("a")))
    assert(partitionEithers1[Int, String](List(Left(1), Left(2))) == (List(1, 2), List()))
    assert(partitionEithers1[Int, String](List(Right("a"), Left(1), Left(2), Right("b"))) == (List(1, 2), List("a", "b")))
    assert(partitionEithers1[Int, String](List(Right("a"))) == (List(), List("a")))

  }

  test("Small library for Either 4") {

    //eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
    def eitherMaybe1[A, B, C](f: B => C, a: Either[A, B]): Option[C] = a match {
      case Left(_) => None
      case Right(x) => Some(f(x))
    }

    assert(eitherMaybe1[Int, Int, String](a => a + " -> ok", Right(1)) == Some("1 -> ok"))
    assert(eitherMaybe1[Int, Int, Int](a => a, Left(1)) == None)

  }

  //either' :: (a -> c) -> (b -> c) -> Either a b -> c
  def either1[A, B, C](f1: => A => C, f2: B => C, x: Either[A, B]): C = x match {
    case Left(x) => f1(x)
    case Right(x) => f2(x)
  }

  test("Small library for Either 5") {

    assert(either1[Int, Int, String](a => a + " -> ko", a => a + " -> ok", Right(1)) == "1 -> ok")
    assert(either1[Int, Int, String](a => a + " -> ko", a => a + " -> ok", Left(2)) == "2 -> ko")

  }

  test("Small library for Either 6") {

    //eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
    def eitherMaybe2[A, B, C](f2: B => C, x: Either[A, B]): Option[C] = x match {
      case Right(_) => Some(either1[A, B, C](???, f2, x))
      case _ => None
    }

    assert(eitherMaybe2[Int, Int, String](a => a + " -> ok", Right(1)) == Some("1 -> ok"))
    assert(eitherMaybe2[Int, Int, String](a => a + " -> ok", Left(2)) == None)

  }

  test("Write your own iterate and unfoldr 1") {
    //myIterate :: (a -> a) -> a -> [a]
    def myIterate[A](f: A => A, a: A): Stream[A] = a #:: myIterate(f, f(a))

    assert {
      myIterate[Int](a => a + 1, 0).take(4) == List(0, 1, 2, 3)
    }
  }

  //myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
  def myUnfoldr[A, B](f: B => Option[(A, B)], b: B): Stream[A] = f(b) match {
    case Some(z) => z._1 #:: myUnfoldr(f, z._2)
    case _ => Stream.empty
  }

  test("Write your own iterate and unfoldr 2") {

    assert {
      myUnfoldr[Int, Int](a => Some((a, a + 1)), 0).take(4) == List(0, 1, 2, 3)
    }

  }

  test("Write your own iterate and unfoldr 3") {
    //betterIterate :: (a -> a) -> a -> [a]
    def betterIterate[A](f: A => A, a: A): Stream[A] = {
      myUnfoldr[A, A](x => Some((x, f(x))), a)
    }

    assert {
      betterIterate[Int](a => a + 1, 0).take(4) == List(0, 1, 2, 3)
    }

  }

  /**
    * data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)
    *
    */

  trait BinaryTree[+A]

  case object Leaf extends BinaryTree[Nothing]

  final case class Node[A](left: BinaryTree[A], node: A, right: BinaryTree[A]) extends BinaryTree[A]

  test("Finally something other than a list!") {
    // unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b

    //TODO lazy
    def unfold[A, B](f: A => Option[(A, B, A)], a: A, count: Int): BinaryTree[B] =
      if (count == 0) Leaf else f(a) match {
        case None => Leaf
        case Some((z1, z2, z3)) => Node(unfold(f, z1, count - 1), z2, unfold(f, z3, count - 1))
      }

    assert(unfold[Int, Int](a => Some((a + 1, a, a + 1)), 0, 2) == Node(Node(Leaf, 1, Leaf), 0, Node(Leaf, 1, Leaf)))

  }
}
