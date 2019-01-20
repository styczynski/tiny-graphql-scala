package parser.exceptions

import parser.schema.types.GraphQLType

final case class InvalidTypeSpecificationError(typeDef: GraphQLType[_], val detailsDescription: String) extends Error(s"Type specification for ${typeDef.getStringName} is not valid: $detailsDescription") {
}
