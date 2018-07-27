import org.scalatest.FunSuite
import MyPredef._
class Chapter7Test extends FunSuite {

  test("RegisteredUser") {

    //    haskell:
    //    module RegisteredUser where
    //    newtype Username = Username String
    //    newtype AccountNumber = AccountNumber Integer
    //    data User = UnregisteredUser | RegisteredUser Username AccountNumber
    //    printUser :: User -> IO ()
    //    printUser UnregisteredUser = putStrLn "UnregisteredUser"
    //    printUser (RegisteredUser (Username name)(AccountNumber acctNum)) = putStrLn $ name ++ " " ++ show acctNum


    final case class Username(name: String)
    final case class AccountNumber(id: Int)

    sealed trait User
    final case class RegisteredUser(username: Username, accountNumber: AccountNumber) extends User
    final case object UnregisteredUser extends User

    def printUser(user: User): Unit = user match {
      case UnregisteredUser => println("UnregisteredUser")
      case RegisteredUser(name: Username, acctNum: AccountNumber) => println(s"$name $acctNum")
    }

    printUser(UnregisteredUser)
    printUser(RegisteredUser(Username("name1"), AccountNumber(1)))

  }

  test("Variety Pack") {
    //    haskell:
    //    k :: (a, b) -> a
    //    k (x, y) = x
    //    k1 = k ((4-1), 10)
    //    k2 = k ("three", (1 + 2))
    //    k3 = k (3, true)

    def k[A](a: (A, _)) = a match {
      case a => a._1
    }

    def k1 = k(4 - 1, 10)

    def k2 = k("three", (1 + 2))

    def k3 = k(3, true)

    assert(k1 == 3)
    assert(k2 == "three")
    assert(k3 == 3)
  }

  test("case") {
    // haskell:
    //    ifEvenAdd2case n = case even n of
    //    true -> n + 2
    //    False -> n
    //
    //    ifEvenAdd2 n = if even n then (n+2) else n

    def ifEvenAdd2(n: Int) = if (even(n)) n + 2 else n

    def ifEvenAdd2case(n: Int) = even(n) match {
      case true => n + 2
      case _ => n
    }

    assert {
      ifEvenAdd2case(2) == ifEvenAdd2(2) && ifEvenAdd2case(2) == 4
    }

  }

  test("Artful Dodgy") {
    // haskell:
    //    dodgy :: Num a => a -> a -> a
    //    dodgy x y = x + y * 10
    //
    //    oneIsOne :: Num a => a -> a
    //    oneIsOne = dodgy 1
    //
    //    oneIsTwo :: Num a => a -> a
    //    oneIsTwo = (flip dodgy) 2
    //
    //    flip :: (a -> b -> c) -> b -> a -> c
    //    flip f x y = f y x

    def flip[A](f: (A, A) => A)(x: A, y: A) = f(y, x)

    def dodgy[A <: Int](x: A, y: A) = x + y * 10

    def oneIsOne[A <: Int](y: A) = dodgy(1, y)

    def oneIsTwo[A <: Int](y: A) = flip(dodgy)(2, y)

    assert(dodgy(1, 1) == 11)

    assert(dodgy(2, 2) == 22)

    assert(dodgy(1, 2) == 21)

    assert(dodgy(2, 1) == 12)

    assert(oneIsOne(1) == 11)

    assert(oneIsOne(2) == 21)

    assert(oneIsTwo(1) == 21)

    assert(oneIsTwo(2) == 22)

    assert(oneIsOne(3) == 31)

    assert(oneIsTwo(3) == 23)
  }

  test("tensDigit") {
    //    tensDigit :: Integral a => a -> a
    //    tensDigit x = d where
    //      xLast = x `div` 10
    //      d = xLast `mod` 10

    def tensDigit(x: Int): Int = (x / 10) % 10

    def tensDigit2(x: Int): Int = {
      import scala.math.Integral.Implicits._
      val (quotient, _) = x /% 10
      quotient % 10
    }

    assert(tensDigit(1) == tensDigit2(1))
    assert(tensDigit(10) == tensDigit2(10))
    assert(tensDigit(111) == tensDigit2(111))

  }

  def foldBool[A](x: A, y: A, b: Boolean): A = b match {
    case true => x
    case _ => y
  }

  test("foldBool") {
    //    haskell
    //    foldBoolGuard :: a -> a -> Bool -> a
    //    foldBoolGuard x y b
    //      | b == true = x
    //      | otherwise = y


    assert {
      foldBool(1, 3, true) == foldBool(1, 3, true) && foldBool(1, 3, true) == 1
    }

    assert {
      foldBool(1, 3, false) == foldBool(1, 3, false) && foldBool(1, 3, false) == 3
    }

  }

  test("Fill in the definition") {
    //haskell
    //    g :: ( Num a, Num b, Num c) => (a -> b) -> (a, c) -> (b, c)
    //    --g f x = (f (fst x),snd x)
    //    g f (a,b) = (t,b)
    //    where t = f a

    def g[A, B, C](f: (A => B))(ac: (A, C)): (B, C) = ac match {
      case (a, b) => (f(a), b)
    }

    assert {
      g((x: Int) => x + 2)((1, 2)) == (3, 2)
    }


  }

  test("foldBool list") {
    //    haskell
    //    map (\x -> bool 'a' 'b' x) [True, False] == ['b','a']

    assert {
      List(true, false).map(foldBool('a', 'b', _)) == List('a', 'b')
    }

  }

}