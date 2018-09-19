import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Properties, _}

class Chapter14Prop extends Properties("ch14") {

  property("3 - plusAssociative") = forAll { (x: Int, y: Int, z: Int) => x + (y + z) == (x + y) + z }

  property("3 - plusCommutative") = forAll { (x: Int, y: Int) => x + y == y + x }

  property("4 - mulAssociative") = forAll { (x: Int, y: Int, z: Int) => x * (y * z) == (x * y) * z }

  property("4 - mulCommutative") = forAll { (x: Int, y: Int) => x * y == y * x }

  val tupleNonZero = for {
    x <- arbitrary[Int]
    y <- Arbitrary.arbitrary[Int].suchThat(_ != 0)
  } yield (x, y)

  property("5 - quot rem div mod") = forAll(tupleNonZero) { case (x, y) => (x / y) * y + (x % y) == x }

  val differentInt = for {
    x <- arbitrary[Byte].suchThat(_ > 1)
    y <- arbitrary[Byte].suchThat(_ > 1).suchThat(_ != x)
  } yield (x, y)

  //TODO property("6 - ^") = forAll(differentInt) { case (x, y) => (x ^ y) != (y ^ x) }


  property("7 - string reverse") = forAll { (x: String) => x.reverse.reverse == x }

  property("9 - foldr") = forAll { (x: List[Int], y: List[Int]) => x.foldRight(y)(_ :: _) == x ++ y }

  val listAndInt = for {
    x <- arbitrary[List[Int]]
    y <- arbitrary[Byte].suchThat(_ <= x.length).suchThat(_ > 0)
  } yield (x, y)


  property("10 - length") = forAll(listAndInt) { case (xs, n) => xs.take(n).length == n }

  property("11 - read show") = forAll { (n: Int) => n.toString.toInt == n }

  def twice[A](f: A => A, a: A): A = f(f(a))

  def fourTimes[A](f: A => A, a: A): A = twice(f, twice(f, a))

  property("idenpotence 1") = forAll { (s: String) => twice(MyPredef.capitalize, s) == s.capitalize && fourTimes(MyPredef.capitalize, s) == s.capitalize }

  def chipers(s: Seq[Char], key: Seq[Char], count: Int = 0): String = s match {
    case Nil => ""
    case x :: xs => (x.toInt + (key(count % key.length).toInt - 'A'.toInt)).toChar + chipers(xs, key, count + 1)
  }

  def unChipers(s: Seq[Char], key: Seq[Char], count: Int = 0): String = s match {
    case Nil => ""
    case x :: xs => (x.toInt - (key(count % key.length).toInt - 'A'.toInt)).toChar + unChipers(xs, key, count + 1)
  }

  val key = "ALLY".toList
  property("Validating ciphers") = forAll { (m: String) => unChipers(chipers(m.toList, key).toList, key) == m }


}
