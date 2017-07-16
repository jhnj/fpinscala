package fpinscala.parsing

import language.higherKinds
import fpinscala.testing._

import scala.util.matching.Regex


trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON


  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._


    def tokenString(s: String): Parser[String] = token(string(s))

    def array: Parser[JArray] = scope("array")(surround(
      tokenString("["),
      separator(json, tokenString(",")).map(l => JArray(l.toIndexedSeq)),
      tokenString("]")
    ))
    def obj: Parser[JSON] = scope("object")(surround(
      tokenString("{"),
      separator(quotedString ** (tokenString(":") *> json) , tokenString(",")).map(l => JObject(l.toMap)),
      tokenString("}")
    ))

    def value: Parser[JSON] =
      scope("null")(tokenString("null").as(JNull)) |
      scope("boolean")(
        tokenString("true").as(JBool(true)) |
        tokenString("false").as(JBool(false))) |
      scope("string")(quotedString.map(s => JString(s))) |
      scope("number")(double.map(d => JNumber(d)))


    def json: Parser[JSON] = obj | array | value

    whitespace *> (obj | array)
  }
}

object TestMyParsers {
  def main(args: Array[String]): Unit = {
    val jsonParser = JSON.jsonParser(MyParsers)
    val json = """
{
  "Parameter" : "Value",
  "Boolean"  : true,
  "Number"   : 30.66,
  "Int" : 1,
  "Array" : [ "A", "B", "C"]
}
"""
    println(MyParsers.run(jsonParser)(json))
  }
}

object TestReferenceParsers {
  def main(args: Array[String]): Unit = {
    val jsonParser = JSON.jsonParser(ParserImplementation)
    val json = """
{
  "Parameter" : "Value",
  "Boolean"  : true,
  "Number"   : 30.66,
  "Int" : 1,
  "Array" : [ "A", "B", :]
}
"""

    println(ParserImplementation.run(jsonParser)(json))
  }
}
