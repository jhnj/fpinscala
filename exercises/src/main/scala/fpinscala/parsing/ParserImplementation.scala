package fpinscala.parsing

import ReferenceParsersUtils._

import scala.util.matching.Regex

object ReferenceParsersUtils {
  type Parser[+A] = Location => Result[A]

  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, isCommitted) => Failure(f(e), isCommitted)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, isCommitted = false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e,c) => Failure(e, c || isCommitted)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a,m) => Success(a,n+m)
      case _ => this
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  def firstNonMatching(input: String, offset: Int, s2: String): Int = {
    var i = 0
    while (i < input.length && i < s2.length) {
      if (input.charAt(i + offset) != s2.charAt(i)) return i
      i += 1
    }
    if (input.length - offset >= s2.length) -1
    else input.length - offset
  }
}

object ParserImplementation extends Parsers[Parser] {
  def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(Location(input)) match {
      case Success(a, _) => Right(a)
      case Failure(parseError, _) => Left(parseError)
    }
  /**
    * 9.14 string, regex, succeed and slice
    */
  def string(s: String): Parser[String] =
    scope("string parsing failed")(l => {
      val nonMatchingIndex = firstNonMatching(l.input, l.offset, s)
      if (nonMatchingIndex == -1)
        Success(s, s.length)
      else
        Failure(l.advanceBy(nonMatchingIndex).toError(s"$s"), nonMatchingIndex != 0)
    })

  def regex(r: Regex): Parser[String] =
    l =>
      r.findPrefixOf(l.inputWithOffset()) match {
        case Some(string) =>
          Success(string, string.length)
        case _ =>
          Failure(l.toError(s"regex $r"), isCommitted = false)
      }

  def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

  def slice[A](p: Parser[A]): Parser[String] =
    l => p(l) match {
      case Success(_, i) => Success(l.inputWithOffset().take(i), i)
      case f: Failure => f
    }

  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    l => p(l).mapError(_.push(l, msg))

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    l => p(l).mapError(_.label(msg))

  def attempt[A](p: Parser[A]): Parser[A] =
    s => p(s).uncommit

  def or[A](x: Parser[A], y: => Parser[A]): Parser[A] =
    s => x(s) match {
      case Failure(e,false) => y(s)
      case r => r
    }

  def flatMap[A,B](f: Parser[A])(g: A => Parser[B]): Parser[B] =
    s => f(s) match {
      case Success(a,n) => g(a)(s.advanceBy(n))
        .addCommit(n != 0)
        .advanceSuccess(n)
      case f@Failure(_,_) => f
    }

  def fail[A](msg: String)(p: Parser[A]): Parser[A] =
    l => Failure(l.toError(msg), isCommitted = true)

}
