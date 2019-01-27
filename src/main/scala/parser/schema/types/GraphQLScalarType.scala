package parser.schema.types

import parser.schema.GraphResolveTrace

final case class GraphQLScalarType(override val name: Option[String] = None, override val isNullableValue: Boolean = true) extends GraphQLType[GraphQLScalarType] {
  override def makeCopy: GraphQLScalarType = copy()
  override def withNullability(shouldBeNullable: Boolean): GraphQLScalarType = copy(isNullableValue = shouldBeNullable)
  override def getTypeKeyword: String = "scalar"
  override def withName(newName: String): GraphQLScalarType = copy(name = Some(newName))
  override def getFormattedString(nestedMode: Boolean, isTop: Boolean): String = {
    name match {
      case Some(nameText) => s"$nameText$getModifiersText"
      case None => ""
    }
  }

  override def onDirectionExtraction(resolveTrace: GraphResolveTrace): GraphQLTypeDirection = GraphQLDirectionUni()
  override def getDirection(resolveTrace: GraphResolveTrace = new GraphResolveTrace()): GraphQLTypeDirection = GraphQLDirectionUni()
}