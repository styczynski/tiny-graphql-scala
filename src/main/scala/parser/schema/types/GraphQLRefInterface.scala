package parser.schema.types

import parser.schema.{GraphQLSchema, GraphResolveTrace}
import parser.exceptions.Failable

final case class GraphQLRefInterface(schema: GraphQLSchema, override val name: Option[String] = None, override val isNullableValue: Boolean = true, fields: Map[String,  GraphQLField] = Map()) extends GraphQLInterface {
  override def makeCopy: GraphQLRefInterface = copy()
  override def withNullability(shouldBeNullable: Boolean): GraphQLRefInterface = copy(isNullableValue = shouldBeNullable)
  override def isNullable: Boolean = isNullableValue
  override def withField(field: GraphQLField): GraphQLRefInterface = copy()
  override def withName(newName: String): GraphQLRefInterface = copy(name = Some(newName))
  override def getFields: Map[String, GraphQLField] = schema.findInterface(name.get).getFields
  override def getTypeKeyword: String = resolve.getTypeKeyword
  override def isAbstract: Boolean = resolve.isAbstract
  def resolve: GraphQLType[GraphQLInterface] = schema.findInterface(name.get).withNullability(isNullableValue)

  override def satisfiesType(graphQLType: GraphQLType[_], resolveTrace: GraphResolveTrace): Failable = resolve.satisfiesType(graphQLType, resolveTrace)
  override def getDirection(resolveTrace: GraphResolveTrace): GraphQLTypeDirection = resolve.getDirection(resolveTrace)
  override def valdiateType(resolveTrace: GraphResolveTrace = new GraphResolveTrace()): Failable = resolve.valdiateType(resolveTrace)
  override def onDirectionExtraction(resolveTrace: GraphResolveTrace): GraphQLTypeDirection = getDirection(resolveTrace)
}