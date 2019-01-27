package parser.exceptions

import parser.schema.types.GraphQLType

final case class SyntaxError(details: String) extends Error(s"syntax error: $details") {
}
