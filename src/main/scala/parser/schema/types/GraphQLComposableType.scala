package parser.schema.types

abstract class GraphQLComposableType[T] extends GraphQLType[T] {
  def getFields: Map[String, GraphQLType[_]]
  def hasField(name: String): Boolean = getField(name).isDefined
  def getField(name: String): Option[GraphQLType[_]] = getFields.get(name)
  def withField(fieldName: String, graphQLType: GraphQLType[_]): GraphQLComposableType[T]
  override def getFormattedString(nestedMode: Boolean, isTop: Boolean): String = {
    val typesString = getFields.foldLeft("")((acc: String, keyValue: (String,  GraphQLType[_])) => {
      val typeText = keyValue._2.toString(nestedMode, isTop = false).split("\\r?\\n").foldLeft("")((acc, line) => acc + (if (acc.isEmpty) "" else "   ")  + line + "\n")
      s"$acc${if (acc.isEmpty) "" else "    "}${keyValue._1}: $typeText"
    })
    name match {
      case Some(nameText) =>  s"""|$nameText {
                                  |    $typesString}""".stripMargin
      case None =>  s"""|{
                        |    $typesString}""".stripMargin
    }
  }
  override def satisfiesType(graphQLType: GraphQLType[_]): Boolean = satisfiesTypeModifiers(graphQLType) && (graphQLType match {
    case composable: GraphQLComposableType[_] => composable.getFields.forall((keyValue: (String, GraphQLType[_])) => {
      getField(keyValue._1) match {
        case Some(fieldType) => if (fieldType.satisfiesType(keyValue._2)) true else throw new Exception(s"$getTypeKeyword $getStringName does not satisfy ${graphQLType.getTypeKeyword} ${graphQLType.getStringName}: Field ${keyValue._1} has type ${keyValue._2.getStringName} which is incompatible with ${fieldType.getTypeKeyword} ${fieldType.getStringName}")
        case None => throw new Exception(s"$getTypeKeyword $getStringName does not satisfy ${graphQLType.getTypeKeyword} ${graphQLType.getStringName}: Missing ${keyValue._1} field")
      }
    })
    case _ => throw new Exception(s"$getTypeKeyword $getStringName does not satisfy ${graphQLType.getTypeKeyword} ${graphQLType.getStringName}: Type mismatch")
  })
}
