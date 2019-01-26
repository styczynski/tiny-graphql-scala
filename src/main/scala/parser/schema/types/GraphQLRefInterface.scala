package parser.schema.types

import parser.schema.GraphQLSchema
import parser.exceptions.Failable

final case class GraphQLRefInterface(schema: GraphQLSchema, override val name: Option[String] = None, override val isNullableValue: Boolean = true, fields: Map[String,  GraphQLField[_]] = Map()) extends GraphQLInterface {
  override def makeCopy: GraphQLRefInterface = copy()
  override def withNullability(shouldBeNullable: Boolean): GraphQLRefInterface = copy(isNullableValue = shouldBeNullable)
  override def isNullable: Boolean = isNullableValue
  override def withField(fieldName: String, field: GraphQLField[_]): GraphQLRefInterface = copy()
  override def withName(newName: String): GraphQLRefInterface = copy(name = Some(newName))
  override def getFields: Map[String, GraphQLField[_]] = schema.findInterface(name.get).getFields
  override def getTypeKeyword: String = resolve.getTypeKeyword
  override def isAbstract: Boolean = resolve.isAbstract
  override def satisfiesType(graphQLType: GraphQLType[_], resolveTrace: Set[(Option[String], Option[String])]): Failable = resolve.satisfiesType(graphQLType, resolveTrace)
  def resolve: GraphQLType[GraphQLInterface] = schema.findInterface(name.get).withNullability(isNullableValue)
}