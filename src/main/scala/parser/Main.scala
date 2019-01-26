package parser

import parser.exceptions.ParserError

object Main extends App {
  println("Parsing started.")
  val code =
      """|
         |type TypeB {
         |  x(a, b, c): String
         |}
         |
         |""".stripMargin
  println(code)
  val parser = SchemaParser()
  try {
    println(parser.parse(code))
  } catch {
    case x: ParserError[_] => println(x)
    case x => throw x
  } finally {
    println("Parsing finished.")
  }
}
