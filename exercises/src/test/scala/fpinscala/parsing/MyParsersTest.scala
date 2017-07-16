package fpinscala.parsing

import fpinscala.parsing.MyParserUtils._
import fpinscala.parsing.MyParsers._
import org.scalatest.EitherValues._
import org.scalatest._

import scala.language.higherKinds

class MyParsersTest extends FunSuite with Matchers {
  test("string") {
    val p = string("A")
    MyParsers.run(p)("A") should be (Right("A"))
  }

  test("listOfN") {
    val list = listOfN(3, string("A"))
    MyParsers.run(list)("AAA") should be (Right(List("A", "A", "A")))
    MyParsers.run(list)("AA") should be ('left)
  }

  test("*>") {
    val parser = string("A") *> string("B")
    MyParsers.run(parser)("AB") should be (Right("B"))
    MyParsers.run(parser)("AA") should be ('left)
  }

  test("*> and <*") {
    val parser = string("A") *> string("B") <* string("A")
    MyParsers.run(parser)("ABA") should be (Right("B"))
    MyParsers.run(parser)("AAA") should be ('left)
  }

  test("or") {
    val parser = string("A") | string("B")
    MyParsers.run(parser)("A") should be (Right("A"))
    MyParsers.run(parser)("B") should be (Right("B"))
    MyParsers.run(parser)("C") should be ('left)
  }

  test("temp") {
    val parser = string("[") *> ((string("A") or string("B")) <* string("]"))
    MyParsers.run(parser)("[B]") should be (Right("B"))
    MyParsers.run(parser)("[A]") should be (Right("A"))
    MyParsers.run(parser)("AAA") should be ('left)
  }


  test("recursive") {
    def leaf: MyParser[String] =
      string("[") *> ((expr | string("1")) <* string("]"))

    def expr = leaf
    MyParsers.run(expr)("[1]").right.value should be ("1")
    MyParsers.run(expr)("[[1]]") should be (Right("1"))
    MyParsers.run(expr)("[[1]") should be ('left)
  }

}

