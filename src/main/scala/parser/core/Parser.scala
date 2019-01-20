package parser.core

import parser.exceptions.ParseError
import parser.exceptions.RuntimeError
import scala.io.Source

abstract class Parser[ParserCoreT <: org.parboiled2.Parser, ResultT] {
  var core: Option[ParserCoreT] = None

  def run(parser: ParserCoreT): ResultT

  def createCore(code: String): ParserCoreT

  def parseFile(filename: String): ResultT = {
    parse(Source.fromFile(filename).mkString)
  }

  def parse(code: String): ResultT = {
    core = Some(createCore(code))
    try {
      core match {
        case Some(parser) => run(parser)
        case None => throw RuntimeError[this.type](new Exception("Core parser is not available"), this)
      }
    } catch {
      case error: org.parboiled2.ParseError => throw ParseError[this.type](error, this)
      case error: Throwable => throw RuntimeError[this.type](error, this)
    }
  }

  def formatParseError(schemaParseError: ParseError[_]): String = {
    core match {
      case Some(parser) => parser.formatError(schemaParseError.error)
      case None => throw RuntimeError[this.type](new Exception("Core parser is not available"), this)
    }
  }
}
