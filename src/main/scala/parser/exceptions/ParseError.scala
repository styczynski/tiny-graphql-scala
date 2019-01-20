package parser.exceptions
import parser.core.Parser

final case class ParseError[ParserT <: Parser[ParserCoreT, ResultT] forSome { type ParserCoreT <: org.parboiled2.Parser; type ResultT } ](error: org.parboiled2.ParseError, schemaParser: ParserT) extends Exception(error.getMessage, error.getCause) {
  override def toString: String = schemaParser.formatParseError(this)
}
