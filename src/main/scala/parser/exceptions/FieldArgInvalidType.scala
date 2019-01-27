package parser.exceptions

import parser.schema.types.GraphQLType

final case class FieldArgInvalidType(fieldName: String, fieldType: GraphQLType[_]) extends Error(
  s"field $fieldName of type ${fieldType.getStringName} is invalid because expected type was input and the actual is ${fieldType.getDirection().getShortName}"
) {
}
