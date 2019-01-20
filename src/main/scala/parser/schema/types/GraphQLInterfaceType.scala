package parser.schema.types

final case class GraphQLInterfaceType(override val name: Option[String] = None, override val isNullableValue: Boolean = true, fields: Map[String,  GraphQLType[_]] = Map()) extends GraphQLComposableType[GraphQLInterfaceType] {
  override def makeCopy: GraphQLInterfaceType = copy()
  override def withNullability(shouldBeNullable: Boolean): GraphQLInterfaceType = copy(isNullableValue = shouldBeNullable)
  override def withField(fieldName: String, graphQLType: GraphQLType[_]): GraphQLInterfaceType = copy(fields = fields + (fieldName -> graphQLType))
  override def withName(newName: String): GraphQLInterfaceType = copy(name = Some(newName))
  override def getFields: Map[String, GraphQLType[_]] = fields
  override def getTypeKeyword = "interface"
  override def isAbstract = true
}
