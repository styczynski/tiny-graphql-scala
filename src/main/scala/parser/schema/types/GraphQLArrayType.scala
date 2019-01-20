package parser.schema.types

final case class GraphQLArrayType(subtype: GraphQLType[_], override val isNullableValue: Boolean = true) extends GraphQLType[GraphQLArrayType](name = None) {
  override def makeCopy: GraphQLArrayType = copy()
  override def withNullability(shouldBeNullable: Boolean): GraphQLArrayType = copy(isNullableValue = shouldBeNullable)
  override def withName(newName: String): GraphQLArrayType = copy()
  override def getName: Option[String] = None
  def getSubtype: GraphQLType[_] = subtype
  def withSubtype(newSubtype: GraphQLType[_]): GraphQLArrayType = copy(subtype = newSubtype)
  override def getFormattedString(nestedMode: Boolean, isTop: Boolean): String = {
    s"[${subtype.toString(nestedMode, isTop=false)}]$getModifiersText"
  }
  override def satisfiesType(graphQLType: GraphQLType[_]): Boolean = satisfiesTypeModifiers(graphQLType) && (graphQLType match {
    case array: GraphQLArrayType => array.getSubtype.satisfiesType(subtype)
    case _ => throw new Exception(s"${getTypeKeyword} $getStringName does not satisfy ${graphQLType.getTypeKeyword} ${graphQLType.getStringName}: Type mismatch")
  })
}

