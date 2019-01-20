package parser.core

import scala.io.Source
import scala.collection.immutable.Seq
import parser.exceptions.ParserError
import org.parboiled2.{ParseError, RuleTrace, Position}
import org.parboiled2.ParseError

abstract class Parser[ParserCoreT <: org.parboiled2.Parser, ResultT] {
  var core: Option[ParserCoreT] = None

  def run(parser: ParserCoreT): ResultT

  def createCore(code: String): ParserCoreT

  def parseFile(filename: String): ResultT = {
    parse(Source.fromFile(filename).mkString)
  }

  def parse(code: String): ResultT = {
    try {
      core = Some(createCore(code))
      run(core.get)
    } catch {
      case error: ParserError[_] => throw error.withParser(core)
      case error: ParseError => throw ParserError(error, error.position, Some(error.traces)).withParser(core)
      case error: Throwable => throw ParserError(error).withParser(core)
    }
  }
}
