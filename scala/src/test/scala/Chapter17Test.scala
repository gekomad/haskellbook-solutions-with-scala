import cats.Applicative
import org.scalatest.FunSuite

class Chapter17Test extends FunSuite {


  test("Applicative Name") {

    /*
      newtype Name = Name String deriving (Eq, Show)
      fmap Name ( Just "cop" )
      --Just (Name "cop")
    * */

    final case class Name(s: String)

    assert(Some("babe").map(Name) == Some(Name("babe")))
    assert(None.map(Name) == None)

  }

  test("Applicative Cow") {
    import cats.implicits._
    /*
     data Cow = Cow {  name:: String, age:: Int, weight :: Int} deriving (Eq, Show)

      noEmpty :: String -> Maybe String
      noEmpty ""  = Nothing
      noEmpty str = Just str

      noNegative :: Int -> Maybe Int
      noNegative n | n >= 0 = Just n
        | otherwise = Nothing
    * */

    final case class Cow(name: String, age: Int, wight: Int)

    object Cow {
      def noEmpty(s: String): Option[String] = s match {
        case "" => None
        case str => Some(str)
      }

      def noNegative(i: Int): Option[Int] = i match {
        case n if n >= 0 => Some(n)
        case _ => None
      }
    }

    val ok = (Cow.noEmpty("cow"), Cow.noNegative(4), Cow.noNegative(100)).mapN(Cow.apply)
    assert(ok == Some(Cow("cow", 4, 100)))

    val ko1 = (Cow.noEmpty("cow"), Cow.noNegative(-4), Cow.noNegative(100)).mapN(Cow.apply)
    assert(ko1 == None)

    val ko2 = (Cow.noEmpty("cow"), Cow.noNegative(4), Cow.noNegative(-100)).mapN(Cow.apply)
    assert(ko2 == None)
  }

  {
    import cats.implicits._
    // liftA2 (+) [1, 2] [3, 4] == [4,5,5,6]

    val o = (List(1, 2), List(3, 4)).mapN(_ + _)
    assert(o == List(4, 5, 5, 6))
  }

  {
    //Person
    final case class Person(name: Name, address: Address)

    final case class Name(n: String)
    final case class Address(n: String)

    def validateLength(maxLen: Int, s: String): Option[String] = if (s.length > maxLen) None else Some(s)

    def mkName(s: String): Option[Name] = validateLength(6, s).map(Name)

    //    def mkAddress(s: String): Option[String] = validateLength(100, s)

    import cats.implicits._
    //  (liftA2 Person (mkName "a wrong looooooooooong name") (Just (Address "farm"))) == Nothing

    val ko = (mkName("a wrong looooooooooong name"), Some(Address("farm"))).mapN(Person)
    assert(ko == None)

    // (liftA2 Person (mkName "babe") (Just (Address "farm"))) == (Just (Person (Name "babe") (Address "farm")))

    val ok = (mkName("babe"), Some(Address("farm"))).mapN(Person)
    assert(ok == Some(Person(Name("babe"), Address("farm"))))
  }

  {
    import cats.implicits._
    //Cow
    final case class Cow(name: String, age: Int, weight: Int)

    def noEmpty(s: String): Option[String] = if (s == "") None else Some(s)

    def noNegative(n: Int): Option[Int] = if (n >= 0) Some(n) else None

    //  (liftA3 Cow (noEmpty "cow") (noNegative (-4)) (noNegative 100)) == Nothing

    val ko = (noEmpty("cow"), noNegative(-4), noNegative(100)).mapN(Cow)
    assert(ko == None)

    //   (liftA3 Cow (noEmpty "cow") (noNegative 4) (noNegative 100)) == (Just (Cow {name = "cow", age = 4, weight = 100}))

    val ok = (noEmpty("cow"), noNegative(4), noNegative(100)).mapN(Cow)
    assert(ok == Some(Cow("cow", 4, 100)))
  }
}
