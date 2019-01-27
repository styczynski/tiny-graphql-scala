package parser.exceptions

import parser.schema.types.GraphQLType

final case class FieldWithArgsNotAllowed(typeValue: GraphQLType[_], field: String, detailedDescription: String) extends Error(
  s"${typeValue.getTypeKeyword} ${typeValue.getStringName} cannot have arguments on field $field: $detailedDescription"
) {
}
