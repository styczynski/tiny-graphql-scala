package parser.core

import scala.io.Source
import scala.collection.immutable.Seq
import parser.exceptions.{ParserError, SyntaxError}
import org.parboiled2.{ErrorFormatter, ParseError, Position, RuleTrace}

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
      case error: ParseError => throw ParserError(SyntaxError(core.get.formatError(error, new ErrorFormatter(
        showExpected = true,
        showPosition = false,
        showLine = false,
        showTraces = false,
        showFrameStartOffset = false,
        expandTabs = 0,
        traceCutOff = 0
      ))), error.position, Some(error.traces)).withParser(core)
      case error: Throwable => throw ParserError(error).withParser(core)
    }
  }
}
