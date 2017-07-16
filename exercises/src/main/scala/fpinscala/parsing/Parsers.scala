package fpinscala.parsing

import java.util.regex.Pattern

import language.higherKinds
import fpinscala.testing._

import scala.util.matching.Regex


trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
  ParserOps[String] = ParserOps(f(a))

  /*************************************************
    * Primitives:
    ************************************************/
  implicit def string(s: String): Parser[String]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  // Always try p2 if p1 fails
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  // Commit to p1 if the first A of p1 passes
  def attempt[A](p1: Parser[A]): Parser[A]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  /****************************************************/




  def succeed[A](a: A): Parser[A]
//    string("").map(_ => a)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List[A]())

  /** 9.7
    *
    */
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = {
    for {
      pa <- p
      pb <- p2
    } yield (pa, pb)
    flatMap(p)(a => map(p2)(b => (a,b)))
  }

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    for {
      pa <- p1
      pb <- p2
    } yield f(pa, pb)
  }

  /** 9.8
    *
    */
  def map[A, B](p1: Parser[A])(f: A => B): Parser[B] = {
    p1.flatMap((a) => succeed(f(a)))
  }


  /** 9.1
    *
    */
  def map2WithProduct[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    product(p1, p2) map { case (a, b) => f(a, b) }
  }
  def many1[A](p: Parser[A]): Parser[List[A]] = {
    map2(many(p), p)((l, a) => a :: l)
  }

  /** 9.4
    *
    */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n > 0) map2(p, listOfN(n - 1, p))(_ :: _)
    else succeed(List[A]())
  }

  def letter: Parser[String] = "[a-zA-Z]".r
  def digit: Parser[String] = """\d""".r
  def whitespace: Parser[String] = "\\s*".r

  /**
    * Skips the second parameter
    */
  def skipRight[A](p: Parser[A], pSkip: => Parser[Any]): Parser[A] = {
    map2(p, slice(pSkip))((a, _) => a)
  }

  /**
    * Skips the first parameter
    */
  def skipLeft[A](pSkip: Parser[Any], p: => Parser[A]): Parser[A] = {
    map2(slice(pSkip), p)((_, a) => a)
  }

  def separator[A, B](p: Parser[A], sep: Parser[B]): Parser[List[A]] = {
    separator1(p, sep) | succeed(List[A]())
  }

  /**
    * Atleast one p with separator sep
    */
  def separator1[A, B](p: Parser[A], sep: Parser[B]): Parser[List[A]] = {
    map2(p, many(skipLeft(sep, p)))((a, b) => a :: b)
  }

  def surround[A](start: Parser[Any], p: => Parser[A], end: Parser[Any]): Parser[A] = {
    start *> p <* end
  }

  /**  9.6
    *  first parse an Int n then listOfN(n, p)
    */
  def parseListOfLen[A](p: Parser[A]): Parser[List[A]] = {
    val reg: Parser[String] ="""\d""".r
    reg.flatMap(s => listOfN(s.toInt, p))
  }

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def quotedString: Parser[String] =
    token("\"" *> ".*?\"".r.map(_.dropRight(1)))

  def doubleString: Parser[String] =
    token("([0-9]*[.])?[0-9]+".r)

  def double: Parser[Double] =
    doubleString.map(_.toDouble)

  /** p followed by whitespace */
  def token[A](p: Parser[A]): Parser[A] =
    p <* whitespace




  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def many1: Parser[List[A]] = self.many1(p)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def *>[B](p2: => Parser[B]): Parser[B] = self.skipLeft(p, p2)
    def <*[B](p2: => Parser[B]): Parser[A] = self.skipRight(p, p2)

    def as[B](b: B): Parser[B] = p.map(_ => b)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = {
      Prop.forAll(in)(a => run(p1)(a) == run(p2)(a))
    }

    def mapLaw[A](p1: Parser[A])(g: Gen[String]): Prop = {
      equal(p1.map(a => a), p1)(g)
    }

//    def succeedLaw[A](a: A)(g: Gen[A]): Prop = {
//      Prop.check(run(succeed(a))(g) == Success(a))
//    }

//    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop = {
//      Prop.forAll(inputs ** Gen.string(100)) { case (input, errorMsg) =>
//        run(label(errorMsg)(p))(input) match {
//          case Failure(_) => true
//          case Success(_, _) => true
//        }
//      }
//    }
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line-1).next
    else ""

  def inputWithOffset(): String = input.drop(offset)
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
  def push(location: Location, msg: String): ParseError = copy(stack = (location, msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latestLoc: Option[Location] =
    latest map (_._1)

  def latest: Option[(Location, String)] =
    stack.lastOption

}
