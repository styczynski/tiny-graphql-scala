package parser.exceptions

import parser.schema.types.GraphQLType

final case class NotCompatibleTypesError(superType: GraphQLType[_], subType: GraphQLType[_], mismatchDescription: String) extends Error(
  s"${superType.getTypeKeyword} ${superType.getStringName} does not satisfy ${subType.getTypeKeyword} ${subType.getStringName}: $mismatchDescription"
) {
}
