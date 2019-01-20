package parser.exceptions

import parser.core.Parser

final case class RuntimeError[ParserT <: Parser[ParserCoreT, ResultT] forSome { type ParserCoreT <: org.parboiled2.Parser; type ResultT } ](error: Throwable, schemaParser: ParserT) extends Exception(error.getMessage, error.getCause) {
  override def toString: String = error.getMessage
}

