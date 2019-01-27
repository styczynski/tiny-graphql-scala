package parser.schema.types

import parser.exceptions.NotCompatibleTypesError
import parser.exceptions.{Fail, Failable, Success}
import parser.schema.GraphResolveTrace

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

  override def onDirectionExtraction(resolveTrace: GraphResolveTrace): GraphQLTypeDirection = subtype.getDirection(resolveTrace)
  override def getDirection(resolveTrace: GraphResolveTrace = new GraphResolveTrace()): GraphQLTypeDirection = subtype.getDirection(resolveTrace)

  override def onTypeValidation(resolveTrace: GraphResolveTrace): Failable = subtype.valdiateType(resolveTrace)
  override def onSatisfactionCheck(graphQLType: GraphQLType[_], resolveTrace: GraphResolveTrace): Failable = graphQLType match {
      case array: GraphQLArrayType => array.getSubtype.satisfiesType(subtype, resolveTrace)
      case refType: GraphQLRefType => onSatisfactionCheck(refType.resolve, resolveTrace)
      case refInterface: GraphQLRefInterface => onSatisfactionCheck(refInterface.resolve, resolveTrace)
      case union: GraphQLUnionType => union.anySubtypeIsSatisfiedBy(this, resolveTrace)
      case _ => Fail(NotCompatibleTypesError(this, graphQLType, "Type mismatch"))
  }

}

