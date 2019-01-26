package parser.schema.types

final case class GraphQLInterfaceType(override val name: Option[String] = None, override val isNullableValue: Boolean = true, fields: Map[String,  GraphQLField[_]] = Map()) extends GraphQLInterface {
  override def makeCopy: GraphQLInterfaceType = copy()
  override def withNullability(shouldBeNullable: Boolean): GraphQLInterfaceType = copy(isNullableValue = shouldBeNullable)
  override def withField(fieldName: String, field: GraphQLField[_]): GraphQLInterfaceType = copy(fields = fields + (fieldName -> field))
  override def withName(newName: String): GraphQLInterfaceType = copy(name = Some(newName))
  override def getFields: Map[String, GraphQLField[_]] = fields
  override def getTypeKeyword = "interface"
  override def isAbstract = true
}
