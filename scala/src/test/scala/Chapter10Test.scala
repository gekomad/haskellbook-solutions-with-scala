import org.scalatest.FunSuite
import MyPredef._

import scala.collection.immutable

class Chapter10Test extends FunSuite {


  test("Understanding Folds 5.a") {
    //foldr (++) [] ["woot", "WOOT", "woot"] == "wootWOOTwoot"

    assert {
      List("woot", "WOOT", "woot").foldRight("")(_ + _) == "wootWOOTwoot"
    }
  }

  test("Understanding Folds 5.b") {
    //foldr max [] (words "fear is the little death") == "the"

    assert {

      words("fear is the little death").foldRight("")(max) == "the"

    }
  }

  test("Understanding Folds 5.c") {
    //foldr (&&) True [False, True] == False

    assert {

      List(false, true).foldRight(true)(_ && _) == false

    }
  }

  test("Understanding Folds 5.d") {
    //foldr (||) True [False, True] == True

    assert {

      List(false, true).foldRight(true)(_ || _) == true

    }
  }

  test("Understanding Folds 5.e") {
    //foldr ((++) . show) "" [1..5] == "12345"

    assert {

      (1 to 5).foldRight("")(_ + _) == "12345"

    }
  }

  test("Understanding Folds 5.f") {
    //foldr (\_ -> const 'a') ' '  [1..5] == 'a'

    assert {

      (1 to 5).foldRight(' ')((_, b) => const('a', b)) == 'a'

    }
  }

  test("Understanding Folds 5.g") {
    //foldr (\_ -> const 0) 0  "tacos" == 0

    assert {

      "tacos".foldRight(0)((_, b) => const(0, b)) == 0

    }
  }

  test("Understanding Folds 5.h") {
    //foldr (flip const ) 0 "burritos" == 0

    assert {

      //      "burritos".foldRight(0)((a, b) => const(b, a)) == 0
      "burritos".foldRight(0)(flip(const[Int], _, _)) == 0

    }
  }

  test("Understanding Folds 5.i") {
    //foldr (flip const) 'z' [1..5] == 'z'

    assert {

      (1 to 5).foldRight('z')(flip(const[Char], _, _)) == 'z'

    }
  }

  // haskell:
  //  data DatabaseItem = DbString String
  //                    | DbNumber Integer
  //                    | DbDate   UTCTime
  //                    deriving (Eq, Ord, Show)
  //
  //  theDatabase :: [DatabaseItem]
  //  theDatabase =
  //      [ DbDate (UTCTime (fromGregorian 1911 5 1)  (secondsToDiffTime 34123))
  //      , DbNumber 9001
  //      , DbString "Hello, world!"
  //      , DbDate (UTCTime
  //            (fromGregorian 1921 5 1)
  //            (secondsToDiffTime 34123))
  //  ]

  import java.time._

  trait DatabaseItem

  final case class DbString(a: String) extends DatabaseItem

  final case class DbNumber(int: Int) extends DatabaseItem

  final case class DbDate(localDateTime: LocalDateTime) extends DatabaseItem

  val d1: LocalDateTime = LocalDateTime.of(1911, Month.MAY, 1, 0, 0).plusSeconds(34123)
  val d2: LocalDateTime = LocalDateTime.of(1921, Month.MAY, 1, 0, 0).plusSeconds(34123)

  val theDatabase: List[DatabaseItem] = List(
    DbDate(d1),
    DbNumber(9001),
    DbString("Hello, world!"),
    DbDate(d2)
  )

  // haskell:
  //  filterDbDate :: [DatabaseItem] -> [UTCTime]
  //  filterDbDate [] = []
  //  filterDbDate ((DbDate x) : xs) = x : (filterDbDate xs)
  //  filterDbDate (_:xs) = filterDbDate xs

  def filterDbDate(db: List[DatabaseItem]): List[LocalDateTime] = db match {
    case Nil => Nil
    case (x: DbDate) :: xs => x.localDateTime :: filterDbDate(xs)
    case _ :: xs => filterDbDate(xs)
  }

  // haskell:
  //  filterDbNumber :: [DatabaseItem] -> [Integer]
  //  filterDbNumber [] = []
  //  filterDbNumber ((DbNumber x) : xs) = x : (filterDbNumber xs)
  //  filterDbNumber (_:xs) = filterDbNumber xs

  def filterDbNumber(db: List[DatabaseItem]): List[Int] = db match {
    case Nil => Nil
    case (x: DbNumber) :: xs => x.int :: filterDbNumber(xs)
    case _ :: xs => filterDbNumber(xs)
  }

  test("Database Processing 1") {

    assert {
      filterDbDate(theDatabase) == List(d1, d2)
    }
  }

  test("Database Processing 2") {
    //    filterDbNumber :: [DatabaseItem] -> [Integer]
    //    filterDbNumber = undefined

    assert {
      filterDbNumber(theDatabase) == List(9001)
    }
  }

  test("Database Processing 3") {
    //haskell:
    //    mostRecent :: [DatabaseItem] -> UTCTime
    //    mostRecent xs = go $ filterDbDate xs
    //    where go :: [UTCTime] -> UTCTime
    //          go [] = error "NoSuchElement"
    //          go (x1:xs1) = foldl max x1 xs1

    def getMostRecent(list: Seq[LocalDateTime]): LocalDateTime = list match {
      case Nil => throw new NoSuchElementException
      case x :: xs => xs.foldLeft(x)((a, b) => if (a.compareTo(b) > 0) a else b)
    }

    def mostRecent(db: List[DatabaseItem]): LocalDateTime = getMostRecent(filterDbDate(theDatabase))

    assert {
      mostRecent(theDatabase) == d2
    }

    {
      val d1 = LocalDateTime.of(1911, Month.MAY, 1, 0, 0)
      val d2 = LocalDateTime.of(1921, Month.MAY, 1, 0, 0)
      val d3 = LocalDateTime.of(2003, Month.MAY, 1, 0, 0)
      val d4 = LocalDateTime.of(2002, Month.MAY, 1, 0, 0)

      assert {
        getMostRecent(List(d1, d2, d3, d4)) == d3
      }

      assertThrows[NoSuchElementException] {
        getMostRecent(List())
      }
    }
  }

  test("Database Processing 4") {
    //    sumDb :: [DatabaseItem] -> Integer
    //    sumDb = undefined

    def sumDb(db: List[DatabaseItem]): Int = filterDbNumber(theDatabase).sum

    assert {
      sumDb(theDatabase) == 9001
    }

  }

  test("Database Processing 5") {

    //    avgDb :: [DatabaseItem] -> Double
    //    avgDb xs = go (filterDbNumber xs)
    //            where go xsn = (fromIntegral $ sum xsn) / (fromIntegral $ length xsn)

    def avgDb(db: List[DatabaseItem]): Int = {
      val list: immutable.Seq[Int] = filterDbNumber(theDatabase)
      list.sum / list.length
    }

    assert {
      avgDb(theDatabase) == 9001
    }
  }

  def fibs: Stream[Int] = 1 #:: fibs.scanLeft(1)(_ + _)

  test("Scans Exercises 1") {

    assert {
      fibs.take(20).toList == List(1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765)
    }
  }

  test("Scans Exercises 2") {

    assert {
      fibs.takeWhile(_ < 100).toList == List(1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89)
    }
  }

  test("Scans Exercises 3") {

    def factorial: Stream[BigInt] = Stream.from(1).scanLeft(BigInt(1))(_ * _)

    assert {
      factorial.take(10).toList == List(1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880)
    }
  }

  val stops = "pbtdkg"
  val vowels = "aeiou"

  test("Chapter Exercises 1 a") {

    val x: immutable.Seq[(Char, Char, Char)] = for {
      a <- stops
      b <- vowels
      c <- stops
    } yield (a, b, c)

    assert {
      x.size == 180
    }
  }

  test("Chapter Exercises 1 b") {

    val x: immutable.Seq[(Char, Char, Char)] = for {
      a <- stops
      b <- vowels
      c <- stops
      if a == 'p'
    } yield (a, b, c)

    assert {
      x.size == 30
    }
  }

  test("Chapter Exercises 1 c") {

    val nouns = List("tree", "sea", "bird")
    val verbs = List("go", "buy", "walk")

    val x: immutable.Seq[(String, String, String)] = for {
      a <- nouns
      b <- verbs
      c <- nouns
    } yield (a, b, c)

    assert {
      x.size == 27
    }
  }

  test("Chapter Exercises 2 - seekritFunc") {
    //    seekritFunc x::String =
    //      div (sum (map length (words x)))
    //         (length (words x))

    def seekritFunc(x: String): Int = words(x).map(_.length).sum / words(x).length

    assert(seekritFunc("aa bb ccc ddddd") == 3)

  }

  test("Rewriting functions using folds - 1 - myOr") {

    def myOr(b: Seq[Boolean]): Boolean = b.foldRight(false)(_ || _)

    assert(myOr(List(true, true)) == true)
    assert(myOr(List(false, false)) == false)
    assert(myOr(List(true, false)) == true)
    assert(myOr(List(false, true, false)) == true)
    assert(myOr(List(false, true)) == true)
    //    assert ( myOr(infiniteTrue) == true )TODO
  }

  test("Rewriting functions using folds - 2 - myAny") {

    def myAny[A](f: A => Boolean, b: List[A]): Boolean = b.foldRight(false)((x, y) => y || f(x))

    assert(myAny(even, List(1, 3, 5)) == false)
    assert(myAny(odd, List(1, 3, 5)) == true)
    assert(myAny(even, List(2, 2)) == true)
    assert(myAny(even, List(1, 2)) == true)
    assert(myAny(even, List(2, 1)) == true)
    assert(myAny(even, List(1, 1)) == false)
    assert(myAny(even, List(1, 2, 1)) == true)
    assert(myAny(odd, List(2, 1, 2)) == true)

  }

  test("Rewriting functions using folds - 3 - myElement") {

    def myElem[A](a: A, b: Seq[A]): Boolean = b.foldRight(false)((x, acc) => x == a || acc)

    assert(myElem(1, 1 to 10) == true)
    assert(myElem(5, 1 to 10) == true)
    assert(myElem(1, 2 to 10) == false)
  }

  test("Rewriting functions using folds - 4 - reverse") {

    def myReverse[A](b: Seq[A]): List[A] = b.foldRight(List.empty[A])((x, acc) => acc ::: List(x))

    assert(myReverse("blah") == "halb".toList)
    assert(myReverse(1 to 3) == List(3, 2, 1))

  }

  test("Rewriting functions using folds - 5 - myMap") {

    def myMap[A, B](f: A => B, b: Seq[A]): List[B] = b.foldRight(List.empty[B])((x, acc) => f(x) :: acc)

    assert(myMap(odd, 1 to 3) == List(true, false, true))
    assert(myMap(even, 1 to 3) == List(false, true, false))

  }

  test("Rewriting functions using folds - 6 - myFilter") {

    def myFilter[A](f: A => Boolean, b: Seq[A]): List[A] = b.foldRight(List.empty[A])((x, acc) => if (f(x)) x :: acc else acc)

    assert(myFilter(odd, 1 to 3) == List(1, 3))
    assert(myFilter(even, 1 to 3) == List(2))

  }

  test("Rewriting functions using folds - 7 - squish") {

    def squish[A](b: List[List[A]]): List[A] = b.foldRight(List.empty[A])((x, acc) => x ::: acc)

    assert(squish(List(List(1, 2), List(3, 4))) == List(1, 2, 3, 4))

  }

  def squishMap[A, B](f: (A => List[B]), xs: List[A]): List[B] = xs.foldRight(List.empty[B])((a, acc) => f(a) ::: acc)

  test("Rewriting functions using folds - 8 - squishMap") {

    assert(squishMap[Int, Int](x => List(1, x, 3), List(2)) == List(1, 2, 3))

    assert {
      squishMap[Char, Char](x => "WO ".toList ++ List(x) ++ " OT ".toList, "blah".toList) == "WO b OT WO l OT WO a OT WO h OT ".toList
    }
  }

  test("Rewriting functions using folds - 9 - squishAgain") {

    def squishAgain[A](xs: List[List[A]]): List[A] = squishMap(((x: List[A]) => x), xs)

    assert(squishAgain(List(List(1, 2), List(3))) == List(1, 2, 3))
    assert(squishAgain(List(List())) == List())

  }

  test("Rewriting functions using folds - 10 - myMaximumBy") {

    def myMaximumBy[A: Ordering](xs: List[A]): A = xs.foldRight(xs.head)((a, acc) => max(a, acc))

    assert(myMaximumBy(List(1, 53, 9001, 10)) == 9001)
    assert(myMaximumBy(List("a", "b")) == "b")
    assert(myMaximumBy(List('a', 'b', 'a')) == 'b')

  }

  test("Rewriting functions using folds - 11 - myMinimumBy") {

    def myMinimumBy[A: Ordering](xs: List[A]): A = xs.foldRight(xs.head)((a, acc) => min(a, acc))

    assert(myMinimumBy(List(1, 53, 9001, 10)) == 1)
    assert(myMinimumBy(List("a", "b")) == "a")
    assert(myMinimumBy(List('a', 'b')) == 'a')
    assert(myMinimumBy(List('b', 'a', 'c')) == 'a')

  }

}
