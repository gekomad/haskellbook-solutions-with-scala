import org.scalatest.FunSuite

import scala.annotation.tailrec


class Chapter8Test extends FunSuite {

  trait DividedResult

  case object DividedByZero extends DividedResult

  final case class Result(res: Int) extends DividedResult

  def dividedBy(num: Int, denom: Int): (DividedResult, Int) = {

    lazy val x = if (num < 0 && denom > 0 || denom < 0 && num > 0) -1 else 1

    def go(n: Int, d: Int, count: Int): (DividedResult, Int) = d match {
      case 0 => (DividedByZero, 0)
      case _ if n < 0 => go(-n, d, count)
      case _ if d < 0 => go(n, -d, count)
      case _ => if (n < d) (Result(count * x), n) else go(n - d, d, count + 1)
    }


    go(num, denom, 0)
  }

  test("2. recursion - sums") {

    //    haskell:
    //    sums :: (Eq a, Num a) => a -> a
    //    sums n = go n
    //      where go count
    //            | count == 0 = 0
    //            | otherwise = count + go (count - 1)

    def sums(n: Int): Int = n match {
      case 0 => 0
      case _ => n + sums(n - 1)

    }

    assert(sums(5) == 15)
  }

  test("3. multiply by sum") {

    def mul(n: Int, m: Int): Int = m match {
      case 0 => 0
      case _ => n + mul(n, m - 1)
    }

    assert(mul(5, 3) == 15)
  }

  test("Fixing dividedBy") {

    //haskell:
    //    data DividedResult =  Result Integer | DividedByZero


    def dv(a: Int, b: Int): DividedResult = b match {
      case 0 => DividedByZero
      case _ => Result(a / b)
    }

    assert(dv(6, 0) == DividedByZero)
    assert(dv(6, 2) == Result(3))

  }

  test("Fixing dividedBy and zero") {

    //haskell:
    //    data DividedResult =  Result Integer | DividedByZero deriving Eq
    //    dividedBy :: Integer -> Integer -> (DividedResult, Integer)
    //    dividedBy num denom = go num denom 0
    //      where go n d count
    //          | d == 0 = (DividedByZero,0)
    //          | n < 0  = go (negate n) d count
    //          | d < 0  = go n (negate d) count
    //          | n < d = (Result( count * if num<0 && denom >0|| denom <0 && num >0 then -1 else 1), n)
    //          | otherwise = go (n - d) d (count + 1)


    assert(dividedBy(6, 2) == (Result(3), 0))
    assert(dividedBy(7, 2) == (Result(3), 1))
    assert(dividedBy(-7, 2) == (Result(-3), 1))
    assert(dividedBy(-7, -2) == (Result(3), 1))
    assert(dividedBy(7, -2) == (Result(-3), 1))
    assert(dividedBy(7, 0) == (DividedByZero, 0))
    assert(dividedBy(-7, 0) == (DividedByZero, 0))

  }

  test("McCarthy 91") {
    // Haskell:
    //   mcCarthy91 :: Integer -> Integer
    //   mcCarthy91 n = go n
    //      where go n
    //      | n > 100 = n - 10
    //      | otherwise = go . go $ n + 11

    def mcCarthy91(n: Int): Int = n match {
      case _ if n > 100 => n - 10
      case _ => mcCarthy91(mcCarthy91(n + 11))
    }

    assert(mcCarthy91(1) == 91)
    assert(mcCarthy91(200) == 190)
  }

  test("Numbers into words") {
    //    haskell:
    //    import Data.List (intersperse)
    //    digitToWord :: Int -> String
    //    digitToWord n = undefined
    //    digits :: Int -> [Int]
    //    digits n = undefined
    //    wordNumber :: Int -> String
    //    wordNumber n = undefined

    def digitToWord(n: Int): String = n match {
      case 0 => "zero"
      case 1 => "one"
      case 2 => "two"
      case 3 => "three"
      case 4 => "four"
      case 5 => "five"
      case 6 => "six"
      case 7 => "seven"
      case 8 => "eight"
      case 9 => "nine"
    }

    //123 -> List(1,2,3)
    @tailrec
    def digits(n: Int, acc: List[Int] = List.empty[Int]): List[Int] = {
      if (n < 10) n :: acc else {
        dividedBy(n, 10) match {
          case (Result(x), y) =>
            digits(x, y :: acc)
          case _ => acc
        }
      }
    }

    //123 -> "one two three"
    def wordNumber(n: Int): String = digits(n).map(digitToWord).mkString(" ")

    assert(wordNumber(123) == "one two three")
  }


}