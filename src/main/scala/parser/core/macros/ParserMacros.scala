package parser.core.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object ParserMacros {
  def parserErr[T](expr: T): T = macro parserErrMacro

  def parserErrMacro(c: Context)(expr: c.Tree): c.universe.Tree = {
    import c.universe._
    expr match {
      case call =>
        q"""
          try {
            $call
          } catch {
            case error: ParserError[_] => throw error
            case error: ParseError => throw ParserError(error, Position(cursor, input), Some(error.traces))
            case error: Throwable => throw ParserError(error, Position(cursor, input))
          }
        """
    }
  }
}
