package parser.exceptions

import parser.schema.types.GraphQLType

final case class MixedTypesDirectionError(superType: GraphQLType[_], subType: GraphQLType[_], detailedDescription: String) extends Error(
  s"${subType.getTypeKeyword} ${subType.getStringName} [${subType.getDirection()}] cannot be used within ${superType.getTypeKeyword} ${superType.getStringName} [${superType.getDirection()}]: $detailedDescription"
) {
}
