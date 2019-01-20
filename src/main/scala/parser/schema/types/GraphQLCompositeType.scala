package parser.schema.types

final case class GraphQLCompositeType(override val name: Option[String] = None, override val isNullableValue: Boolean = true, fields: Map[String, GraphQLType[_]] = Map(), typeInterface: Option[GraphQLComposableType[_]] = None) extends GraphQLComposableType[GraphQLCompositeType] {
  override def makeCopy: GraphQLCompositeType = copy()
  override def withNullability(shouldBeNullable: Boolean): GraphQLCompositeType = copy(isNullableValue = shouldBeNullable)
  override def withField(fieldName: String, graphQLType: GraphQLType[_]): GraphQLCompositeType = copy(fields = fields + (fieldName -> graphQLType))
  override def withName(newName: String): GraphQLCompositeType = copy(name = Some(newName))
  override def withInterface(newTypeInterface: Option[GraphQLComposableType[_]]): GraphQLCompositeType = newTypeInterface match {
    case Some(newTypeInterfaceValue) => if(newTypeInterfaceValue.isAbstract) copy(typeInterface = newTypeInterface) else
      throw new Exception(s"Type $getName cannot implement non abstract type ${newTypeInterfaceValue.getName}")
    case None => copy(typeInterface = newTypeInterface)
  }
  override def getInterface: Option[GraphQLComposableType[_]] = typeInterface
  override def getFields: Map[String, GraphQLType[_]] = fields
  override def validateType: Boolean = typeInterface match {
    case Some(typeInterfaceValue) => satisfiesType(typeInterfaceValue)
    case None => true
  }
}
