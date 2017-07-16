package fpinscala.parsing

import scala.util.matching.Regex
import MyParserUtils._
import fpinscala.parsing.ReferenceParsersUtils.Success

/**  9.12
  *  Own implementation of parser
  */
object MyParserUtils {
  type MyParser[+A] = Location => Either[ParseError, (A, Int)]
}

object MyParsers extends Parsers[MyParser] {
  def run[A](p: MyParser[A])(input: String): Either[ParseError,A] =
    p(Location(input)) match {
      case Right((a, _))  =>
        Right(a)
      case err: Left[ParseError, A] => err
    }

  def string(s: String): MyParser[String] =
    (location: Location) =>
      if (location.inputWithOffset().startsWith(s)) {
        Right((s, s.length))
      } else {
        Left(location.toError(s"${location.inputWithOffset()} does not match $s"))
      }

  def regex(r: Regex): MyParser[String] =
    (location: Location) =>
      r.findPrefixOf(location.inputWithOffset()) match {
        case Some(string) =>
          Right((string, string.length))
        case _ => Left(location.toError(s"${location.inputWithOffset()} doesn't match with $r"))
      }

  def slice[A](p: MyParser[A]): MyParser[String] =
    (location: Location) =>
      p(location) match {
        case Right((_, advanced)) =>
          Right(location.inputWithOffset().take(advanced), advanced)
        case Left(parseError) => Left(parseError)
      }

  def label[A](msg: String)(p: MyParser[A]): MyParser[A] =
    (location: Location) =>
      p(location).left map {
        case ParseError((loc, _) :: x, otherFailures) => ParseError((loc, msg) :: x, otherFailures)
      }

  def scope[A](msg: String)(p: MyParser[A]): MyParser[A] =
    (location: Location) =>
      p(location).left map { parseError => parseError.push(location, msg)}

  def flatMap[A, B](p: MyParser[A])(f: (A) => MyParser[B]): MyParser[B] =
    (location: Location) =>
      p(location) match {
        case Right((a, advanced)) =>
          val res = f(a)(location.advanceBy(advanced))
          res match {
            case Right((a2, i)) => Right((a2, i + advanced))
            case _ => res
          }
        case Left(parseError) => Left(parseError)
      }

  def attempt[A](p1: MyParser[A]): MyParser[A] = p1

  def or[A](p1: MyParser[A], p2: => MyParser[A]): MyParser[A] =
    (location: Location) =>
      p1(location) match {
        case Left(_) => p2(location)
        case a => a
      }

  def succeed[A](a: A): MyParser[A] =
    (_: Location) =>
      Right((a, 0))
}