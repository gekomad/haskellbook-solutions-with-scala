import org.scalatest.FunSuite

import scala.annotation.tailrec
import scala.collection.immutable

class Chapter11Test extends FunSuite {

  //  data Price = Price Integer deriving (Eq, Show)
  //  data Manufacturer = Mini| Mazda | Tata deriving (Eq, Show)
  //  data Airline = PapuAir | CatapultsR'Us| TakeYourChancesUnited deriving (Eq, Show)
  //  data Vehicle = Car Manufacturer Price| Plane Airline deriving (Eq, Show)
  //
  //  myCar :: Vehicle
  //  myCar = Car Mini (Price 14000)
  //  urCar = Car Mazda (Price 20000)
  //  clownCar = Car Tata (Price 7000)
  //  doge = Plane PapuAir

  final case class Price(p: Int)

  trait Manufacturer

  final case object Mini extends Manufacturer

  final case object Mazda extends Manufacturer

  final case object Tata extends Manufacturer

  trait Airline

  final case object PapuAir extends Airline

  final case object CatapultsR_Us extends Airline

  final case object TakeYourChancesUnited extends Airline

  trait Vehicle

  final case class Car(manufacturer: Manufacturer, price: Price) extends Vehicle

  final case class Plane(airline: Airline) extends Vehicle

  val myCar: Vehicle = Car(Mini, Price(14000))
  val urCar: Vehicle = Car(Mazda, Price(20000))
  val clownCar: Vehicle = Car(Tata, Price(7000))
  val doge: Vehicle = Plane(PapuAir)

  //////////////////////////////////////////////////////////////////////////////////
  /**
    *
    * data OperatingSystem =
    * GnuPlusLinux
    * | OpenBSDPlusNevermindJustBSDStill
    * | Mac
    * | Windows
    * deriving (Eq, Show)
    * *
    */
  trait OperatingSystem

  case object GnuPlusLinux extends OperatingSystem

  case object OpenBSDPlusNevermindJustBSDStill extends OperatingSystem

  case object Mac extends OperatingSystem

  case object Windows extends OperatingSystem

  /**
    * data ProgrammingLanguage =
    * Haskell
    * | Agda
    * | Idris
    * | PureScript
    * deriving (Eq, Show)
    * *
    **/
  trait ProgrammingLanguage

  case object Haskell extends ProgrammingLanguage

  case object Agda extends ProgrammingLanguage

  case object Idris extends ProgrammingLanguage

  case object PureScript extends ProgrammingLanguage

  /**
    * data Programmer =
    * Programmer { os :: OperatingSystem
    * , lang :: ProgrammingLanguage }
    * deriving (Eq, Show)
    * *
    */
  case class Programmer(os: OperatingSystem, lang: ProgrammingLanguage)


  /**
    * allOperatingSystems :: [OperatingSystem]
    * allOperatingSystems =
    * [ GnuPlusLinux
    * , OpenBSDPlusNevermindJustBSDStill
    * , Mac
    * , Windows
    * ]
    */
  val allOperatingSystems: Seq[OperatingSystem] = Seq(GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows)
  /**
    * allLanguages :: [ProgrammingLanguage]
    * allLanguages = [Haskell, Agda, Idris, PureScript]
    * *
    * allProgrammers :: [Programmer]
    * allProgrammers = [Programmer {os=x,lang=y} | x <- allOperatingSystems, y<-allLanguages]
    */

  val allLanguages: Seq[ProgrammingLanguage] = Seq(Haskell, Agda, Idris, PureScript)

  val allProgrammers = for {
    x <- allOperatingSystems
    y <- allLanguages
  } yield (x, y)

  test("Exercises: Vehicles 2") {

    def isCar(v: Vehicle): Boolean = v match {
      case Car(_, _) => true
      case _ => false
    }

    def isPlane(v: Vehicle): Boolean = v match {
      case Plane(_) => true
      case _ => false
    }

    def areCars(v: Seq[Vehicle]): Seq[Boolean] = v.map(isCar)

    assert(isCar(myCar))
    assert(!isCar(doge))

    assert(!isPlane(myCar))
    assert(isPlane(doge) == true)

    assert(areCars(Seq(myCar, urCar)) == Seq(true, true))
    assert(areCars(Seq(clownCar, doge)) == Seq(true, false))

  }

  def getManu(v: Vehicle): Manufacturer = v match {
    case car: Car => car.manufacturer
    case _ => throw new Exception
  }

  test("Exercises: Vehicles 3") {
    assert(getManu(myCar) == Mini)
  }

  test("Exercises: Vehicles 4") {
    assertThrows[Exception](getManu(doge))
  }

  test("Exercises: Vehicles 5") {
    final case class Plane(airline: Airline, size: Int) extends Vehicle
    val doge = Plane(PapuAir, 100)
    assert(doge.size == 100)
  }

  test("Programmers") {
    allProgrammers.length == 16
  }

  /**
    * data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)
    *
    */

  trait BinaryTree[+A]

  case object Leaf extends BinaryTree[Nothing]

  final case class Node[A](left: BinaryTree[A], node: A, right: BinaryTree[A]) extends BinaryTree[A]


  test("BinaryTree") {

    //    mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
    //    mapTree _ Leaf = Leaf
    //    mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

    def mapTree[A, B](f: A => B, tree: BinaryTree[A]): BinaryTree[B] = tree match {

      case Node(l, n, r) => Node(mapTree(f, l), f(n), mapTree(f, r))
      case _ => Leaf
    }

    val testTree1 = Node(Node(Leaf, 3, Leaf), 1, Node(Leaf, 4, Leaf))

    assert {
      mapTree((a: Int) => a + 1, testTree1) == Node(Node(Leaf, 4, Leaf), 2, Node(Leaf, 5, Leaf))
    }
  }

  test("BinaryTree preorder") {

    //    preorder :: BinaryTree a -> [a]
    //    preorder Leaf = []
    //    preorder (Node left a right) = [a] ++ (preorder left) ++ (preorder right)

    def preorder[A](tree: BinaryTree[A]): Seq[A] = tree match {
      case Leaf => Nil
      case Node(l, n, r) => Seq(n) ++ preorder(l) ++ preorder(r)
    }

    val testTree = Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf))

    assert {
      preorder(testTree) == Seq(2, 1, 3)
    }

  }

  def inorder[A](tree: BinaryTree[A]): Seq[A] = tree match {
    case Leaf => Nil
    case Node(l, n, r) => inorder(l) ++ Seq(n) ++ inorder(r)
  }


  test("BinaryTree inorder") {

    //    inorder :: BinaryTree a -> [a]
    //    inorder Leaf = []
    //    inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

    val testTree = Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf))

    assert {
      inorder(testTree) == Seq(1, 2, 3)
    }

  }

  test("BinaryTree postorder") {

    //    postorder :: BinaryTree a -> [a]
    //    postorder Leaf = []
    //    postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

    def postorder[A](tree: BinaryTree[A]): Seq[A] = tree match {
      case Leaf => Nil
      case Node(l, n, r) => postorder(l) ++ postorder(r) ++ Seq(n)
    }

    val testTree = Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf))

    assert {
      postorder(testTree) == Seq(1, 3, 2)
    }
  }

  test("Write foldr for BinaryTree") {

    def foldTree[A, B](f: (A, B) => B, x: B, tree: BinaryTree[A]): B = tree match {
      case Leaf => x
      case Node(l, n, r) => l match {
        case Leaf => f(n, foldTree(f, x, r))
        case Node(_, n1, _) =>
          f(n1, f(n, foldTree(f, x, r)))
      }
    }

    val testTree = Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf))


    assert {
      foldTree((a: Int, b: Int) => a + b, 0, testTree) == 6
    }

  }

  test("Vigenère Ciphers") {

    def ciphers(s: Seq[Char], key: Seq[Char], count: Int = 0): String = s match {
      case Nil => ""
      case x :: xs => (x.toInt + (key(count % key.length).toInt - 'A'.toInt)).toChar + ciphers(xs, key, count + 1)
    }

    def unCiphers(s: Seq[Char], key: Seq[Char], count: Int = 0): String = s match {
      case Nil => ""
      case x :: xs => (x.toInt - (key(count % key.length).toInt - 'A'.toInt)).toChar + unCiphers(xs, key, count + 1)
    }

    val m = "MEET AT DAWN"

    val key = "ALLY".toList
    assert {
      unCiphers(ciphers(m.toList, key).toList, key) == m
    }
  }

  test("isSubsequenceOf") {
    //isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool

    def isSubsequenceOf[A](sub: Seq[A], s: Seq[A]): Boolean = {

      @tailrec
      def contains(a: A, s: Seq[A]): Boolean = s match {
        case Nil => false
        case x :: _ if x == a => true
        case _ :: xs => contains(a, xs)
      }

      sub.map(contains(_, s)).foldLeft(true)(_ && _)

    }

    assert(isSubsequenceOf("blah".toList, "blahwoot".toList))
    assert(isSubsequenceOf("blah".toList, "wootblah".toList))
    assert(isSubsequenceOf("blah".toList, "wboloath".toList))
    assert(!isSubsequenceOf("blah".toList, "wootbla".toList))
  }

  test("capitalizeWords") {
    // capitalizeWords :: String -> [(String, String)]

    def capitalizeWords[A](s: String): Seq[(String, String)] = MyPredef.words(s).map(z => (z, z.capitalize))

    assert(capitalizeWords("hello world") == List(("hello", "Hello"), ("world", "World")))
  }

  def capitalizeWord(s: String): String = {
    def go(s: List[Char]): List[Char] = s match {
      case Nil => Nil
      case x :: xs if x != ' ' => x.toUpper :: xs
      case x :: xs => x :: go(xs)
    }

    go(s.toList) mkString
  }

  test("capitalizeWord") {

    assert(capitalizeWord("hello world") == "Hello world")
    assert(capitalizeWord(" hello") == " Hello")

  }

  test("capitalizeParagraph") {


    def capitalizeParagraph(st: String): String = {

      def go(s: List[String]) = s match {
        case Nil => ""
        case x :: xs =>
          val ss = capitalizeWord(x) + xs.foldLeft("")((a, b) => capitalizeWord(a) + "." + capitalizeWord(b))
          if (st.endsWith(".")) ss + "." else ss
      }

      go(MyPredef.mySplit(st, "\\.").toList)

    }

    assert(capitalizeParagraph("blah. woot ha.") == "Blah. Woot ha.")
    assert(capitalizeParagraph("blah. woot ha") == "Blah. Woot ha")
    assert(capitalizeParagraph("blah") == "Blah")
    assert(capitalizeParagraph("blah.") == "Blah.")
    assert(capitalizeParagraph("") == "")

  }

  test("Phone exercise") {

    type Digit = Char
    type Presses = Int


    //    data DaPhone = DaPhone
    case object DaPhone {

      val reverseMap: Map[Digit, List[(Digit, Presses)]] = Map(
        '1' -> List(('1' -> 1)),

        '.' -> List(('#' -> 1)),
        ',' -> List(('#' -> 2)),

        ' ' -> List(('0' -> 1)),
        '+' -> List(('0' -> 2)),
        '_' -> List(('0' -> 3)),
        '0' -> List(('0' -> 4)),

        'a' -> List(('2' -> 1)),
        'b' -> List(('2' -> 2)),
        'c' -> List(('2' -> 3)),
        '2' -> List(('2' -> 4)),

        'd' -> List(('3' -> 1)),
        'e' -> List(('3' -> 2)),
        'f' -> List(('3' -> 3)),
        '3' -> List(('3' -> 4)),

        'g' -> List(('4' -> 1)),
        'h' -> List(('4' -> 2)),
        'i' -> List(('4' -> 3)),
        '4' -> List(('4' -> 4)),

        'j' -> List(('5' -> 1)),
        'k' -> List(('5' -> 2)),
        'l' -> List(('5' -> 3)),
        '5' -> List(('5' -> 4)),

        'm' -> List(('6' -> 1)),
        'n' -> List(('6' -> 2)),
        'o' -> List(('6' -> 3)),
        '6' -> List(('6' -> 4)),

        'p' -> List(('7' -> 1)),
        'q' -> List(('7' -> 2)),
        'r' -> List(('7' -> 3)),
        's' -> List(('7' -> 4)),
        '7' -> List(('7' -> 5)),

        't' -> List(('8' -> 1)),
        'u' -> List(('8' -> 2)),
        'v' -> List(('8' -> 3)),
        '8' -> List(('8' -> 4)),

        'w' -> List(('9' -> 1)),
        'x' -> List(('9' -> 2)),
        'y' -> List(('9' -> 3)),
        'z' -> List(('9' -> 4)),
        '4' -> List(('9' -> 5)),

        'A' -> List(('*' -> 1), ('2' -> 1)),
        'B' -> List(('*' -> 1), ('2' -> 2)),
        'C' -> List(('*' -> 1), ('2' -> 3)),

        'D' -> List(('*' -> 1), ('3' -> 1)),
        'E' -> List(('*' -> 1), ('3' -> 2)),
        'F' -> List(('*' -> 1), ('3' -> 3)),

        'G' -> List(('*' -> 1), ('4' -> 1)),
        'H' -> List(('*' -> 1), ('4' -> 2)),
        'I' -> List(('*' -> 1), ('4' -> 3)),

        'J' -> List(('*' -> 1), ('5' -> 1)),
        'K' -> List(('*' -> 1), ('5' -> 2)),
        'L' -> List(('*' -> 1), ('5' -> 3)),

        'M' -> List(('*' -> 1), ('6' -> 1)),
        'N' -> List(('*' -> 1), ('6' -> 2)),
        'O' -> List(('*' -> 1), ('6' -> 3)),

        'P' -> List(('*' -> 1), ('7' -> 1)),
        'Q' -> List(('*' -> 1), ('7' -> 2)),
        'R' -> List(('*' -> 1), ('7' -> 3)),
        'S' -> List(('*' -> 1), ('7' -> 4)),

        'T' -> List(('*' -> 1), ('8' -> 1)),
        'U' -> List(('*' -> 1), ('8' -> 2)),
        'V' -> List(('*' -> 1), ('8' -> 3)),

        'W' -> List(('*' -> 1), ('9' -> 1)),
        'X' -> List(('*' -> 1), ('9' -> 2)),
        'Y' -> List(('*' -> 1), ('9' -> 3)),
        'Z' -> List(('*' -> 1), ('9' -> 4))
      )

    }

    val convo = List(
      "Wanna play 20 questions",
      "Ya",
      "U 1st haha",
      "Lol ok. Have u ever tasted alcohol lol",
      "Lol ya",
      "Wow ur cool haha. Ur turn",
      "Ok. Do u think I am pretty Lol",
      "Lol ya",
      "Haha thanks just making sure rofl ur turn")

    //reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
    def reverseTaps(daPhone: DaPhone.type, char: Char): Seq[(Digit, Presses)] = daPhone.reverseMap(char)


    // cellPhonesDead :: DaPhone    -> String  -> [(Digit, Presses)]
    //    def cellPhonesDead(daPhone: DaPhone.type, string: String): Seq[(Digit, Presses)] = string.flatMap(reverseTaps(daPhone, _))
    def cellPhonesDead(daPhone: DaPhone.type, string: String): Seq[(Digit, Presses)] =
      string.foldLeft(List.empty[(Digit, Presses)])((b, a) => b ++ reverseTaps(daPhone, a))

    //    fingerTaps :: [(Digit, Presses)] -> Presses
    def fingerTaps(l: Seq[(Digit, Presses)]): Presses = l.foldLeft(0)(_ + _._2)

    //    mostPopularLetter :: String -> Char
    def mostPopularLetter(string: String): (Char, Int) = {
      require(!string.isEmpty)

      @tailrec
      def go(string: Seq[Char], char: Char, count: Int): Int = string match {
        case Nil => count
        case x :: xs if x == char => go(xs, char, count + 1)
        case _ :: xs => go(xs, char, count)
      }

      val s = string.map(a => (a, go(string.toList, a, 0))).sortBy(_._2).reverse
      s.head
    }

    def mostPopularWord(string: String): String = {
      require(!string.isEmpty)

      @tailrec
      def go(string: Seq[String], s: String, count: Int): Int = string match {
        case Nil => count
        case x :: xs if x == s => go(xs, s, count + 1)
        case _ :: xs => go(xs, s, count)
      }

      val s = MyPredef.words(string).map(a => (a, go(MyPredef.words(string), a, 0))).sortBy(_._2).reverse
      s.head._1
    }

    assert(reverseTaps(DaPhone, 'a') == List(('2' -> 1)))

    assert(reverseTaps(DaPhone, 'A') == List(('*' -> 1), ('2' -> 1)))

    assert(cellPhonesDead(DaPhone, "aa") == List(('2' -> 1), ('2' -> 1)))
    assert(cellPhonesDead(DaPhone, "aA") == List(('2' -> 1), ('*' -> 1), ('2' -> 1)))
    assert(fingerTaps(cellPhonesDead(DaPhone, "Wanna play 20 questions")) == 50)

    //    question 3. How many times do digits need to be pressed for each message?
    assert {
      convo.map(a => (a, fingerTaps(cellPhonesDead(DaPhone, a)))) == List(("Wanna play 20 questions", 50), ("Ya", 5), ("U 1st haha", 17),
        ("Lol ok. Have u ever tasted alcohol lol", 81), ("Lol ya", 15), ("Wow ur cool haha. Ur turn", 49),
        ("Ok. Do u think I am pretty Lol", 58), ("Lol ya", 15), ("Haha thanks just making sure rofl ur turn", 80))
    }

    assert(fingerTaps(List(('a' -> 1), ('b' -> 2), ('c' -> 1))) == 4)

    //    question 4 What was the most popular letter for each message? What was its cost?
    assert {
      convo.map(a => (a, mostPopularLetter(a))) == List(("Wanna play 20 questions", ('n', 3)), ("Ya", ('a', 1)), ("U 1st haha", ('a', 2)),
        ("Lol ok. Have u ever tasted alcohol lol", (' ', 7)), ("Lol ya", ('a', 1)), ("Wow ur cool haha. Ur turn", (' ', 5)),
        ("Ok. Do u think I am pretty Lol", (' ', 7)), ("Lol ya", ('a', 1)), ("Haha thanks just making sure rofl ur turn", (' ', 7)))
    }

    //    question 5 What was the most popular letter overall? What was the most popular word?
    assert(mostPopularLetter(convo.foldLeft("")(_ + _)) == (' ', 33))
    assert(mostPopularWord(convo.foldLeft("")(_ + " " + _)) == "Lol")

    assert(mostPopularLetter("Aaa")._1 == 'a')
    assert(mostPopularLetter("acaba")._1 == 'a')
    assert(mostPopularLetter("aaA")._1 == 'a')
    assert(mostPopularLetter("AaA")._1 == 'A')
    assert(mostPopularWord("aaa bbb aaa") == "aaa")


  }

  test("Hutton’s Razor") {
    //    data Expr = Lit Integer | Add Expr Expr

    trait Expr
    final case class Lit(int: Int) extends Expr
    final case class Add(expr1: Expr, expr2: Expr) extends Expr

    def eval(expr: Expr): Int = expr match {
      case Add(a: Lit, b: Lit) => a.int + b.int
    }

    assert(eval(Add(Lit(1), Lit(9001))) == 9002)

    // printExpr :: Expr -> String

    def printExpr(expr: Expr): String = expr match {
      case Lit(a) => a.toString
      case Add(a, b) => printExpr(a) + " + " + printExpr(b)
    }

    assert(printExpr(Add(Lit(1), Lit(9001))) == "1 + 9001")

    val a1 = Add(Lit(9001), Lit(1))
    val a2 = Add(a1, Lit(20001))
    val a3 = Add(Lit(1), a2)
    assert(printExpr(a3) == "1 + 9001 + 1 + 20001")

  }

}
