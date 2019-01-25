package parser.schema.types

final case class GraphQLEnumType(override val name: Option[String] = None, values: Set[String] = Set(), override val isNullableValue: Boolean = true) extends GraphQLType[GraphQLEnumType] {
  override def makeCopy: GraphQLEnumType = copy()
  override def withNullability(shouldBeNullable: Boolean): GraphQLEnumType = copy(isNullableValue = shouldBeNullable)
  override def withName(newName: String): GraphQLEnumType = copy(name = Some(newName))
  override def getTypeKeyword: String = "enum"
  def withValue(value: String): GraphQLEnumType = copy(values = values + value)
  override def getFormattedString(nestedMode: Boolean, isTop: Boolean): String = {
    val valuesString = values.foldLeft("")((acc: String, value: String) => {
      s"$acc${if (acc.isEmpty) "" else "\n    "}$value"
    })
    name match {
      case Some(nameText) =>  s"""|$nameText {
                                  |    $valuesString
                                  |}""".stripMargin
      case None =>  s"""|{
                        |    $valuesString
                        |}""".stripMargin
    }
  }
}