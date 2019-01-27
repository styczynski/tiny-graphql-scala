package parser.schema.types

import parser.schema.GraphResolveTrace
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
  def resolve: GraphQLType[_] = schema.findType(name.get).withNullability(isNullableValue)

  override def getDirection(resolveTrace: GraphResolveTrace): GraphQLTypeDirection = resolve.getDirection(resolveTrace)
  override def satisfiesType(graphQLType: GraphQLType[_], resolveTrace: GraphResolveTrace): Failable = resolve.satisfiesType(graphQLType, resolveTrace)
  override def valdiateType(resolveTrace: GraphResolveTrace = new GraphResolveTrace()): Failable = resolve.valdiateType(resolveTrace)
  override def onDirectionExtraction(resolveTrace: GraphResolveTrace): GraphQLTypeDirection = getDirection(resolveTrace)
}