import MyPredef.infiniteTrue
import MyPredef.infiniteFalse
import org.scalatest.FunSuite
import MyPredef._

import scala.collection.immutable

class Chapter9Test extends FunSuite {

  val firstSen = "Tyger Tyger, burning bright\n"
  val secondSen = "In the forests of the night\n"
  val thirdSen = "What immortal hand or eye\n"
  val fourthSen = "Could frame thy fearful symmetry?"
  val sentences: String = firstSen ++ secondSen + thirdSen + fourthSen


  test("eftInt") {

    //haskell:
    //    eftInt :: Int -> Int -> [Int]
    //      eftInt start stop = go stop start []
    //       where go from to acc
    //            | from == to  = from : acc
    //            | otherwise   = go  (from -1)  to (from : acc)
    def eftInt(start: Int, stop: Int): List[Int] = {

      def go(from: Int, to: Int, acc: List[Int] = Nil): List[Int] =
        if (from == to) from :: acc else
          go(from - 1, to, from :: acc)

      go(stop, start, List.empty[Int])

    }

    assert(eftInt(10, 12) == List(10, 11, 12))

  }

  test("eftBool") {

    //    haskell
    //    eftBool :: Bool -> Bool -> [Bool]
    //    eftBool True False = [True, False]
    //    eftBool True True = [True]
    //    eftBool False False = [False]
    //    eftBool a b = [a , b]
    def eftBool(start: Boolean, stop: Boolean): List[Boolean] = (start, stop) match {

      case (true, false) => List(true, false)
      case (true, true) => List(true)
      case (false, false) => List(false)
      case (a, b) => List(a, b)

    }

    assert(eftBool(start = false, stop = false) == List(false))
    assert(eftBool(start = true, stop = true) == List(true))

  }

  test("Thy Fearful Symmetry") {

    //    haskell:
    //    thyFearfulSymmetry :: String -> [String]
    //    thyFearfulSymmetry s = go s []
    //      where go s acc
    //            | s == ""      = acc
    //            | otherwise    =  go (dropWhile( == ' ') (dropWhile (/= ' ') s)) (acc ++ [(takeWhile (/= ' ') s )])

    def myWords(s: String, acc: List[String] = List.empty[String]): List[String] = s.isEmpty match {
      case true => acc
      case false => myWords(s.dropWhile(_ != ' ').dropWhile(_ == ' '), acc ::: List(s.takeWhile(_ != ' ')))
    }

    assert(myWords("all i wanna do is have some fun") == List("all", "i", "wanna", "do", "is", "have", "some", "fun"))
  }

  test("PoemLines") {

    def myLines(s: String, acc: List[String] = List.empty[String]): List[String] = s.isEmpty match {
      case true => acc
      case false => myLines(s.dropWhile(_ != '\n').dropWhile(_ == '\n'), acc ::: List(s.takeWhile(_ != '\n')))
    }

    assert(myLines(sentences) == List("Tyger Tyger, burning bright", "In the forests of the night", "What immortal hand or eye", "Could frame thy fearful symmetry?"))
  }

  test("Thy Fearful Symmetry & PoemLines") {

    def go(s: String, sep: Char, acc: List[String] = List.empty[String]): List[String] = s.isEmpty match {
      case true => acc
      case false => go(s.dropWhile(_ != sep).dropWhile(_ == sep), sep, acc ::: List(s.takeWhile(_ != sep)))
    }

    assert(go("all i wanna do is have some fun", ' ') == List("all", "i", "wanna", "do", "is", "have", "some", "fun"))
    assert(go(sentences, '\n') == List("Tyger Tyger, burning bright", "In the forests of the night", "What immortal hand or eye", "Could frame thy fearful symmetry?"))
  }

  val mySqr: immutable.IndexedSeq[Double] = for {
    x <- 1 to 5
  } yield Math.pow(x, 2)

  val myCube: immutable.IndexedSeq[Double] = for {
    y <- 1 to 5
  } yield Math.pow(y, 3)


  test("Square Cube 1") {
    val o = for {
      x <- mySqr
      y <- myCube
    } yield (x, y)

    assert(o == List((1.0, 1.0), (1.0, 8.0), (1.0, 27.0), (1.0, 64.0), (1.0, 125.0), (4.0, 1.0), (4.0, 8.0), (4.0, 27.0), (4.0, 64.0), (4.0, 125.0), (9.0, 1.0), (9.0, 8.0), (9.0, 27.0), (9.0, 64.0), (9.0, 125.0), (16.0, 1.0), (16.0, 8.0), (16.0, 27.0), (16.0, 64.0), (16.0, 125.0), (25.0, 1.0), (25.0, 8.0), (25.0, 27.0), (25.0, 64.0), (25.0, 125.0)))
  }

  test("Square Cube 2") {
    val o = for {
      x <- mySqr
      y <- myCube
      if x < 50 && y < 50
    } yield (x, y)

    assert(o == List((1.0, 1.0), (1.0, 8.0), (1.0, 27.0), (4.0, 1.0), (4.0, 8.0), (4.0, 27.0), (9.0, 1.0), (9.0, 8.0), (9.0, 27.0), (16.0, 1.0), (16.0, 8.0), (16.0, 27.0), (25.0, 1.0), (25.0, 8.0), (25.0, 27.0)))
  }

  test("Square Cube 3") {
    val o = for {
      x <- mySqr
      y <- myCube
      if (x < 50 && y < 50)
    } yield (x, y)

    assert(o.length == 15)
  }

  test("foldBool") {
    //    haskell
    //    map (\x -> bool 'a' 'b' x) [True, False] == ['b','a']

    def foldBool[A](x: A, y: A, b: Boolean): A = b match {
      case true => y
      case _ => x
    }

    assert {
      List(true, false).map(foldBool('a', 'b', _)) == List('b', 'a')
    }


  }

  test("Filtering 1") {
    // filter (\x -> mod x 3 == 0) [1..30] == [3, 6, 9, 12, 15, 18, 21, 24, 27, 30]
    assert {
      (1 to 30).filter(_ % 3 == 0) == List(3, 6, 9, 12, 15, 18, 21, 24, 27, 30)
    }
  }

  test("Filtering 2") {
    // (length $ filter (\x -> mod x 3 == 0) [1..30]) == 10
    assert {
      //      (1 to 30).count(_ % 3 == 0) == 10
      (1 to 30).filter(_ % 3 == 0).length == 10
    }
  }

  test("Filtering 3") {
    // filter( \x -> not (elem x ["the", "a", "an"])) (words "the brown dog was a goof") == ["brown", "dog", "was", "goof"]
    assert {
      "the brown dog was a goof".split(" ").filter(!List("the", "a", "an").contains(_)).toList == List("brown", "dog", "was", "goof")
    }
  }

  test("Zipping exercises 1") {
    // haskell:
    //    myZip :: [a] -> [b] -> [(a, b)]
    //    myZip [] _ = []
    //    myZip _ [] = []
    //    myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

    def myZip[A, B](a: List[A], b: List[B]): List[(A, B)] = (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (x1 :: xs1, x2 :: xs2) => (x1, x2) :: myZip(xs1, xs2)
    }

    assert {
      myZip(List(1, 2, 3), List('a', 'b')) == List((1, 'a'), (2, 'b'))
    }
  }

  def myZipWith[A, B, C](f: (A, B) => C, a: List[A], b: List[B]): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (x1 :: xs1, x2 :: xs2) => f(x1, x2) :: myZipWith(f, xs1, xs2)
  }

  test("Zipping exercises 2") {

    assert {
      myZipWith((a: Int, b: Int) => a + b, List(1, 2, 3), List(1, 1)) == List(2, 3)
    }
  }

  test("Zipping exercises 3") {

    //map
    assert {
      myZipWith((a: Int, b: Char) => (a, b), List(1, 2, 3), List('a', 'b')) == List((1, 'a'), (2, 'b'))
    }
  }

  test("Chapter Exercises 2") {
    assert {
      ("HbEfLrLxO,".filter(_.isUpper == true)) == "HELLO"
    }
  }

  test("Chapter Exercises 3") {
    val n = "julie"
    assert {
      (n.head toUpper) + n.tail == "Julie"
    }
  }

  test("Chapter Exercises 4") {

    def go(n: List[Char]): String = n match {
      case Nil => ""
      case x :: xs => x.toUpper + go(xs)
    }

    assert {
      go("woot".toList) == "WOOT"
    }
  }

  test("Chapter Exercises 5") {
    val n = "julie"
    assert {
      (n.head toUpper) == 'J'
    }
  }

  test("Ciphers") {

    def ciphers(s: List[Char]): String = s match {
      case Nil => ""
      case x :: xs => (x.toInt - 'a'.toInt).toChar + ciphers(xs)
    }

    def unCiphers(s: List[Char]): String = s match {
      case Nil => ""
      case x :: xs => (x.toInt + 'a'.toInt).toChar + unCiphers(xs)
    }

    val m = "hello"

    assert {
      unCiphers(ciphers(m.toList).toList) == m
    }
  }

  // direct recursion, not using (&&)
  def myAnd(b: Seq[Boolean]): Boolean = b match {
    case Nil => true
    case false :: _ => false
    case _ :: xs => myAnd(xs)
    case false #:: _ => false
  }

  def myOr(b: Seq[Boolean]): Boolean = b match {
    case Nil => false
    case true :: _ => true
    case _ :: xs => myOr(xs)
    case true #:: _ => true
  }

  def myAny[A](f: A => Boolean, b: List[A]): Boolean = b match {
    case Nil => false
    case x :: xs => if (f(x)) true else myAny(f, xs)
  }

  test("myOr") {
    assert(myOr(List(true, true)))
    assert(!myOr(List(false, false)))
    assert(myOr(List(true, false)))
    assert(myOr(List(false, true)))
    assert(myOr(infiniteTrue))
  }


  test("myAnd") {
    assert(myAnd(List(true, true)) == true)
    assert(myAnd(List(false, false)) == false)
    assert(myAnd(List(true, false)) == false)
    assert(myAnd(infiniteFalse) == false)
  }

  test("myAny") {

    assert(myAny(even, List(1, 2)) == true)
    assert(myAny(even, List(1, 1)) == false)

  }

  test("myElem") {

    def myElem[A](a: A, b: List[A]): Boolean = b match {
      case Nil => false
      case x :: _ if x == a => true
      case _ :: xs => myElem(a, xs)
    }

    assert(myElem(1, (1 to 10).toList) == true)
    assert(myElem(5, (1 to 10).toList) == true)
    assert(myElem(1, (2 to 10).toList) == false)

  }

  test("myReverse") {

    def myReverse[A](b: List[A]): List[A] = b match {
      case Nil => Nil
      case x :: xs => myReverse(xs) ::: List(x)
    }

    assert(myReverse((1 to 3).toList) == List(3, 2, 1))

  }

  test("squish") {

    //haskell:
    //    squish :: [[a]] -> [a]
    //    squish [] = []
    //    squish [x] = x
    //    squish (x : xs) = x ++ squish xs

    def squish[A](xs: List[List[A]]): List[A] = xs match {
      case Nil => Nil
      case List(x) => x
      case x :: xs => x ::: squish(xs)
    }

    assert(squish(List(List(1, 2), List(3))) == List(1, 2, 3))
    assert(squish(List(List())) == List())

  }

  // haskell:
  //    squishMap :: (a -> [b]) -> [a] -> [b]
  //    squishMap _ [] = []
  //    squishMap f (x : xs) = f x ++ squishMap f xs

  def squishMap[A, B](f: A => List[B], xs: List[A]): List[B] = xs match {
    case Nil => Nil
    case x :: xs => f(x) ::: squishMap(f, xs)
  }

  test("squishMap") {

    assert(squishMap(((x: Int) => List(1, x, 3)), List(2)) == List(1, 2, 3))

    assert(squishMap(((x: Char) => ("WO " + x + " HOO ").toList), "123".toList) == "WO 1 HOO WO 2 HOO WO 3 HOO ".toList)

  }

  test("squish again") {

    //haskell:
    //    squishAgain :: [[a]] -> [a]
    //    squishAgain [] = []
    //    squishAgain [x] = x
    //    squishAgain (x : xs) = x ++ squishMap (\x -> x) xs

    def squishAgain[A](xs: List[List[A]]): List[A] = squishMap(((x: List[A]) => x), xs)

    assert(squishAgain(List(List(1, 2), List(3))) == List(1, 2, 3))
    assert(squishAgain(List(List())) == List())

  }

  test("myMaximumBy") {
    import scala.math.Ordered._

    //haskell:
    //    myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
    //    myMaximumBy _ (x:[]) = x
    //    myMaximumBy f (x:xs) = go f xs x
    //
    //    go :: (a -> a -> Ordering) -> [a] -> a -> a
    //    go _ [] m = m
    //    go f (x : xs) m = go f xs (if (f x m == GT) then x else m)

    def myMaximumBy[A: Ordering](xs: List[A]): A = {

      def go[A: Ordering](xs: List[A], m: A): A = xs match {
        case Nil => m
        case x :: xs => go(xs, max(m,x))
      }

      xs match {
        case Nil => throw new Exception
        case a :: Nil => a
        case x :: xs => go(xs, x)
      }
    }

    assert(myMaximumBy(List(1, 53, 9001, 10)) == 9001)
    assert(myMaximumBy(List("a", "b")) == "b")
    assert(myMaximumBy(List('a', 'b')) == 'b')

  }

  test("myMinimumBy") {

    //haskell:
    //    myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
    //    myMaximumBy _ (x:[]) = x
    //    myMaximumBy f (x:xs) = go f xs x
    //
    //    go :: (a -> a -> Ordering) -> [a] -> a -> a
    //    go _ [] m = m
    //    go f (x : xs) m = go f xs (if (f x m == LT) then x else m)

    def myMinimumBy[A: Ordering](xs: List[A]): A = {

      def go[A: Ordering](xs: List[A], m: A): A = xs match {
        case Nil => m
        case x :: xs => go(xs, MyPredef.min(x, m))
      }

      xs match {
        case Nil => throw new Exception
        case a :: Nil => a
        case x :: xs => go(xs, x)
      }
    }

    assert(myMinimumBy(List(1, 53, 9001, 10)) == 1)
    assert(myMinimumBy(List("a", "b")) == "a")
    assert(myMinimumBy(List('a', 'b')) == 'a')

  }

}