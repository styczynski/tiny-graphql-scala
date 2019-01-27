package parser

import parser.exceptions.ParserError

object Main extends App {
  println("Parsing started.")
  val code =
      """|
         |input Inp {
         |  w: Int!
         |}
         |
         |type Type {
         |  x(z: [[ID!]]!, q: [[String]!], p: [Inp!]): String
         |}
         |""".stripMargin
  println(code)
  val parser = SchemaParser()
  try {
    val schema = parser.parse(code)
  } catch {
    case x: ParserError[_] => println(x.getMessage())
    case x => throw x
  } finally {
    println("Parsing finished.")
  }
}
