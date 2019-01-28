package parser

import parser.core.Parser
import parser.requests.types.QueryFieldRoot
import parser.requests.RequestParserCore

final case class RequestParser() extends Parser[RequestParserCore, QueryFieldRoot] {
 def run(parser: RequestParserCore): QueryFieldRoot = {
  val output = parser.Input.run().get
  output
 }
 def createCore(code: String): RequestParserCore = new RequestParserCore(code)
}