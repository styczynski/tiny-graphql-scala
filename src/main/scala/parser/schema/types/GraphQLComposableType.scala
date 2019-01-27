package parser.schema.types

import parser.exceptions._
import parser.schema.GraphResolveTrace

abstract class GraphQLComposableType[T] extends GraphQLType[T] {
  def getFields: Map[String, GraphQLField]

  def hasField(name: String): Boolean = getField(name).isDefined
  def getField(name: String): Option[GraphQLField] = getFields.get(name)
  def withField(field: GraphQLField): GraphQLComposableType[T]
  def withField(fieldName: String, typeValue: GraphQLType[_]): GraphQLComposableType[T] = {
    withField(GraphQLField(fieldName, Map(), Some(typeValue)))
  }
  def withField(fieldName: String, args: Map[String, GraphQLType[_]], typeValue: GraphQLType[_]): GraphQLComposableType[T] = {
    withField(GraphQLField(fieldName, args, Some(typeValue)))
  }
  override def getFormattedString(nestedMode: Boolean, isTop: Boolean): String = {
    val typesString = getFields.foldLeft("")((acc: String, keyValue: (String, GraphQLField)) => {
      val typeText = keyValue._2.toString(nestedMode, isTop = false).split("\\r?\\n").foldLeft("")((acc, line) => acc + (if (acc.isEmpty) "" else "   ") + line + "\n")
      s"$acc${if (acc.isEmpty) "" else "    "}$typeText"
    })
    name match {
      case Some(nameText) => s"""|$nameText {
                                 |    $typesString}""".stripMargin
      case None => s"""|{
                       |    $typesString}""".stripMargin
    }
  }

  override def onTypeValidation(resolveTrace: GraphResolveTrace): Failable = getFields.foldLeft(Success(): Failable)((acc, keyValue) => {
    acc && keyValue._2.getType.valdiateType(resolveTrace) && (
      if(keyValue._2.getType.getDirection(resolveTrace) !~ this.getDirection(resolveTrace))
        Fail(MixedTypesDirectionError(this, keyValue._2.getType, s"Direction mismatch on key ${keyValue._1}"))
      else Success()
    ) && (getDirection(resolveTrace) match {
      case GraphQLDirectionInput() => if(keyValue._2.getArgs.isEmpty) Success() else Fail(FieldWithArgsNotAllowed(this, keyValue._1, "The type is input type"))
      case _ => keyValue._2.validateType(resolveTrace)
    })
  }) && (getInterface match {
    case Some(typeInterfaceValue) => satisfiesType(typeInterfaceValue, resolveTrace)
    case None => Success()
  })


  override def onSatisfactionCheck(graphQLType: GraphQLType[_], resolveTrace: GraphResolveTrace): Failable = graphQLType match {
      case composable: GraphQLComposableType[_] =>
        composable.getFields.foldLeft(Success(): Failable)((acc: Failable, keyValue: (String, GraphQLField)) => {
          getField(keyValue._1) match {
            case Some(fieldType) => acc && (
                fieldType.satisfiesTypeByReturn(keyValue._2, resolveTrace)
                //|| Fail(NotCompatibleTypesError(this, graphQLType, s"Field ${keyValue._1} has type ${keyValue._2.getType.getStringName} which is incompatible with ${fieldType.getType.getTypeKeyword} ${fieldType.getType.getStringName}"))
              ) && fieldType.satisfiesTypeByArgs(keyValue._2, resolveTrace)
            case None => Fail(NotCompatibleTypesError(this, graphQLType, s"Missing field ${keyValue._1}"))
          }
        })
      case refType: GraphQLRefType => onSatisfactionCheck(refType.resolve, resolveTrace)
      case union: GraphQLUnionType => union.anySubtypeIsSatisfiedBy(this, resolveTrace)
      case _ => Fail(NotCompatibleTypesError(this, graphQLType, "Type mismatch"))
  }
}
