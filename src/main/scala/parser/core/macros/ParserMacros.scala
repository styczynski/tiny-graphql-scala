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
            case error: Throwable => throw new ParseError(Position(cursor, input), Position(cursor, input), RuleTrace(Nil, RuleTrace.Fail("valid input: "+error.getMessage)) :: Nil)
          }
        """
    }
  }
}
