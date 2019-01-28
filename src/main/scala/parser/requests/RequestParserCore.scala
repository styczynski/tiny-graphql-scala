package parser.requests

import scala.language.implicitConversions
import shapeless.{::, HNil}
import org.parboiled2._
import parser.exceptions._
import parser.core.macros.ParserMacros.parserErr
import parser.requests.types.QueryField
import parser.requests.types.QueryFieldRoot
import parser.requests.types.QueryFieldLeaf
import parser.requests.types.QueryFieldBlock
import parser.schema.types._
import parser.schema.{AnyIdentifier, GraphQLSchema}

class RequestParserCore(val input: ParserInput, val env: Option[GraphQLSchema] = None) extends Parser {

  def Input: Rule1[QueryFieldRoot] = rule { push(QueryFieldRoot()) ~ QueryBlock ~ EOI }

  def Letters: Rule0 = rule { oneOrMore(CharPredicate.AlphaNum) }

  def Identifier: Rule1[String] = rule { atomic(capture(Letters)) }

  def KeywordQuery: Rule0 = rule { atomic("query") }

  def Whitespaces: Rule0 = rule {
    quiet(zeroOrMore(str(" ") | str("\n") | str("\r") | str("\t")))
  }

  def Field: Rule1[QueryFieldLeaf] = rule {
    Whitespaces ~
      Identifier ~>
      ((ident: String) => QueryFieldLeaf(Some(ident))) ~
      Whitespaces
  }

  def FieldBlock: Rule1[QueryFieldBlock] = rule {
    Whitespaces ~
      Identifier ~>
      ((ident: String) => QueryFieldBlock(Some(ident))) ~
      Whitespaces ~
      '{' ~
      Whitespaces ~
      zeroOrMore(
        (
          FieldBlock ~> ((block: QueryFieldBlock, subfield: QueryFieldBlock) => block.withField(subfield))
        )
        | (
          Field ~> ((block: QueryFieldBlock, subfield: QueryFieldLeaf) => block.withField(subfield))
        )
      ) ~
      Whitespaces ~
      '}' ~
      Whitespaces
  }

  def QueryBlock: Rule[QueryFieldRoot :: HNil, QueryFieldRoot :: HNil] = rule {
    Whitespaces ~
      KeywordQuery ~
      Whitespaces ~
      '{' ~
      Whitespaces ~
      oneOrMore(
        (
          FieldBlock ~> ((root: QueryFieldRoot, subfield: QueryFieldBlock) => root.withField(subfield))
        )
        | (
          Field ~> ((root: QueryFieldRoot, subfield: QueryFieldLeaf) => root.withField(subfield))
        )
      ) ~ Whitespaces ~
      '}' ~
      Whitespaces
  }
}