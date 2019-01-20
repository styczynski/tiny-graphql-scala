import parser.SchemaParser
import parser.exceptions.ParseError
import parser.exceptions.RuntimeError

object Main extends App {
  println("Parsing started.")
  val code =
      """|
         |type CustomNullability {
         |    x: String!
         |    y: Int!
         |    z: Float
         |}
         |""".stripMargin
  println(code)
  val parser = SchemaParser()
  try {
    println(parser.parse(code))
  } catch {
    case error: ParseError[_] => println(error)
    case error: RuntimeError[_] => println(error)
  } finally {
    println("Parsing finished.")
  }
}