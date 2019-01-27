package parser.schema.types

import parser.schema.GraphResolveTrace
import parser.exceptions._

final case class GraphQLCompositeType(override val name: Option[String] = None, override val isNullableValue: Boolean = true, fields: Map[String, GraphQLField] = Map(), typeInterface: Option[GraphQLComposableType[_]] = None) extends GraphQLComposableType[GraphQLCompositeType] {
  override def makeCopy: GraphQLCompositeType = copy()
  override def withNullability(shouldBeNullable: Boolean): GraphQLCompositeType = copy(isNullableValue = shouldBeNullable)
  override def withField(field: GraphQLField): GraphQLCompositeType = copy(fields = fields + (field.key -> field))
  override def withName(newName: String): GraphQLCompositeType = copy(name = Some(newName))
  override def withInterface(newTypeInterface: Option[GraphQLComposableType[_]]): GraphQLCompositeType =  copy(typeInterface = newTypeInterface)
  override def getInterface: Option[GraphQLComposableType[_]] = typeInterface
  override def getFields: Map[String, GraphQLField] = fields
  override def onDirectionExtraction(resolveTrace: GraphResolveTrace): GraphQLTypeDirection = GraphQLDirectionOutput()
}
