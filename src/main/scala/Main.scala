import parser.SchemaParser
import parser.exceptions.ParserError

object Main extends App {
  println("Parsing started.")
  val code =
      """|interface MyInterface {
         |    data: [Int]
         |}
         |
         |interface MyInterface2 {
         |    fields: [MyInterface]
         |}
         |
         |type MyInterface2Impl {
         |    fields: [MyInterface]
         |}
         |
         |type TypeImpls implements MyInterface {
         |    fields: [MyInterface2Impl]
         |}
         |
         |""".stripMargin
  println(code)
  val parser = SchemaParser()
  try {
    println(parser.parse(code))
  } catch {
    case error: Throwable => println(error)
  } finally {
    println("Parsing finished.")
  }
}