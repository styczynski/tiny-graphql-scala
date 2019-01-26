package parser.schema.types

import parser.schema.GraphQLSchema
import parser.exceptions.Failable

final case class GraphQLRefType(val schema: GraphQLSchema, override val name: Option[String] = None, override val isNullableValue: Boolean = true) extends GraphQLType[GraphQLRefType] {
  override def withName(newName: String): GraphQLRefType = copy(name = Some(newName))
  override def isNullable: Boolean = isNullableValue
  override def makeCopy: GraphQLRefType = copy()
  override def withNullability(shouldBeNullable: Boolean): GraphQLRefType = copy(isNullableValue = shouldBeNullable)
  override def getInterface: Option[GraphQLComposableType[_]] = resolve.getInterface
  override def getName: Option[String] = name
  override def getTypeKeyword: String = resolve.getTypeKeyword
  override def isAbstract: Boolean = resolve.isAbstract
  override def getFormattedString(nestedMode: Boolean, isTop: Boolean): String = resolve.getFormattedString(nestedMode, isTop)
  override def validateType: Failable = resolve.validateType
  override def satisfiesType(graphQLType: GraphQLType[_], resolveTrace: Set[(Option[String], Option[String])]): Failable = resolve.satisfiesType(graphQLType, resolveTrace)
  def resolve: GraphQLType[_] = schema.findType(name.get).withNullability(isNullableValue)
}