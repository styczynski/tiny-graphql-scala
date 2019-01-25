package parser

import schema.{GraphQLSchema, SchemaParserCore}
import core.Parser

final case class SchemaParser() extends Parser[SchemaParserCore, GraphQLSchema] {
 def run(parser: SchemaParserCore): GraphQLSchema = {
  val output = parser.Input.run().get
  output.validate
  output
 }
 def createCore(code: String): SchemaParserCore = new SchemaParserCore(code)
}