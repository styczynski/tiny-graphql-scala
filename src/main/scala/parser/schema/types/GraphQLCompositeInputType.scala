package parser.schema.types

import parser.exceptions._
import parser.schema.GraphResolveTrace

final case class GraphQLCompositeInputType(override val name: Option[String] = None, override val isNullableValue: Boolean = true, fields: Map[String, GraphQLField] = Map(), typeInterface: Option[GraphQLComposableType[_]] = None) extends GraphQLComposableType[GraphQLCompositeInputType] {
  override def makeCopy: GraphQLCompositeInputType = copy()
  override def withNullability(shouldBeNullable: Boolean): GraphQLCompositeInputType = copy(isNullableValue = shouldBeNullable)
  override def withField(field: GraphQLField): GraphQLCompositeInputType = copy(fields = fields + (field.key -> field))
  override def withName(newName: String): GraphQLCompositeInputType = copy(name = Some(newName))
  override def getFields: Map[String, GraphQLField] = fields
  override def getTypeKeyword = "input"

  override def onDirectionExtraction(resolveTrace: GraphResolveTrace): GraphQLTypeDirection = GraphQLDirectionInput()

}
