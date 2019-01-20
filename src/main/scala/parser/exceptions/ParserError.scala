package parser.exceptions
import scala.collection.immutable.Seq
import org.parboiled2.{ParseError, Position, RuleTrace, Parser}

final case class ParserError[T <: Throwable](
  error: T,
  position: Position = Position(0,0,0),
  ruleTraces: Option[Seq[RuleTrace]] = None,
  parser: Option[Parser] = None
) extends Error(error.getMessage) {
  def getError: T = error
  private def getParseError: ParseError = ParseError(position, position, ruleTraces match {
      case Some(value) => value
      case None => RuleTrace(Nil, RuleTrace.Fail(error.getMessage)) :: Nil
    })
  override def getMessage: String = parser match {
    case Some(parserValue) => parserValue.formatError(getParseError)
    case None => error.getMessage
  }
  def withParser(newParser: Option[Parser]): ParserError[T] = copy(parser = newParser)
}
