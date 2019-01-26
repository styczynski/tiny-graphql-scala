package parser.schema.types

import parser.exceptions.NotCompatibleTypesError
import parser.exceptions.{Fail, Success, Failable}

abstract class GraphQLComposableType[T] extends GraphQLType[T] {
  def getFields: Map[String, GraphQLField[_]]

  def hasField(name: String): Boolean = getField(name).isDefined
  def getField(name: String): Option[GraphQLField[_]] = getFields.get(name)
  def withField(fieldName: String, field: GraphQLField[_]): GraphQLComposableType[T]
  def withField(fieldName: String, typeValue: GraphQLType[_]): GraphQLComposableType[T] = {
    withField(fieldName, GraphQLField(Nil, typeValue))
  }

  override def getFormattedString(nestedMode: Boolean, isTop: Boolean): String = {
    val typesString = getFields.foldLeft("")((acc: String, keyValue: (String, GraphQLField[_])) => {
      val typeText = keyValue._2.toString(nestedMode, isTop = false).split("\\r?\\n").foldLeft("")((acc, line) => acc + (if (acc.isEmpty) "" else "   ") + line + "\n")
      s"$acc${if (acc.isEmpty) "" else "    "}${keyValue._1}: $typeText"
    })
    name match {
      case Some(nameText) => s"""|$nameText {
                                 |    $typesString}""".stripMargin
      case None => s"""|{
                       |    $typesString}""".stripMargin
    }
  }

  override def satisfiesType(graphQLType: GraphQLType[_], resolveTrace: Set[(Option[String], Option[String])]): Failable = {
    val isInTrace = resolveTrace((getName, graphQLType.getName))
    lazy val isMatching = graphQLType match {
      case composable: GraphQLComposableType[_] =>
        composable.getFields.foldLeft(Success(): Failable)((acc: Failable, keyValue: (String, GraphQLField[_])) => {
          getField(keyValue._1) match {
            case Some(fieldType) => acc && (fieldType.getType.satisfiesType(keyValue._2.getType, resolveTrace + ((getName, graphQLType.getName))) || Fail(NotCompatibleTypesError(this, graphQLType, s"Field ${keyValue._1} has type ${keyValue._2.getType.getStringName} which is incompatible with ${fieldType.getType.getTypeKeyword} ${fieldType.getType.getStringName}")))
            case None => Fail(NotCompatibleTypesError(this, graphQLType, s"Missing field ${keyValue._1}"))
          }
        })
      case refType: GraphQLRefType => satisfiesType(refType.resolve, resolveTrace)
      case union: GraphQLUnionType => union.anySubtypeIsSatisfiedBy(this, resolveTrace + ((getName, graphQLType.getName)))
      case _ => Fail(NotCompatibleTypesError(this, graphQLType, "Type mismatch"))
    }
    satisfiesTypeModifiers(graphQLType) && (if (isInTrace) Success() else isMatching)
  }
}
