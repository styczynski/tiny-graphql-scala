package parser

import parser.exceptions.ParserError

object Main extends App {
  println("Parsing started.")
  val code =
      """|type TypeA implements InterfaceA {
         |  x: TypeB
         |  y: InterfaceA
         |}
         |
         |interface InterfaceA {
         |  x: InterfaceB
         |  y: InterfaceA
         |}
         |
         |interface InterfaceB {
         |  x: InterfaceA
         |  y: InterfaceB
         |}
         |
         |union UnionA = TypeA | TypeB
         |
         |type TypeB implements InterfaceB {
         |  x: TypeA
         |  y: UnionA
         |}
         |
         |""".stripMargin
  println(code)
  val parser = SchemaParser()
  try {
    println(parser.parse(code))
  } catch {
    case x: ParserError[_] => throw (x.getError)
    case x => throw x
  } finally {
    println("Parsing finished.")
  }
}
