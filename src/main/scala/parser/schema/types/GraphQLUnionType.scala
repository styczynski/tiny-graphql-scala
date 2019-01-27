package parser.schema.types

import parser.exceptions._
import parser.schema.GraphResolveTrace

final case class GraphQLUnionType(override val name: Option[String] = None, types: Set[GraphQLType[_]] = Set(), override val isNullableValue: Boolean = true) extends GraphQLType[GraphQLUnionType] {
  override def makeCopy: GraphQLUnionType = copy()
  override def withNullability(shouldBeNullable: Boolean): GraphQLUnionType = copy(isNullableValue = shouldBeNullable)
  override def withName(newName: String): GraphQLUnionType = copy(name = Some(newName))
  override def getTypeKeyword: String = "union"
  def withType(newType: GraphQLType[_]): GraphQLUnionType = copy(types = types + newType)
  override def getFormattedString(nestedMode: Boolean, isTop: Boolean): String = {
    val valuesString = types.foldLeft("")((acc: String, typeValue: GraphQLType[_]) => {
      s"$acc${if (acc.isEmpty) "" else " | "}${typeValue.getStringName}"
    })
    name match {
      case Some(nameText) => s"$nameText = $valuesString"
      case None => s"= $valuesString"
    }
  }
  def everySubtypeSatisfies(graphQLType: GraphQLType[_], resolveTrace: GraphResolveTrace): Failable = {
    types.foldLeft(Success(): Failable)((acc: Failable, typeValue: GraphQLType[_]) => {
      lazy val isMatching = typeValue.satisfiesType(graphQLType, resolveTrace)
      acc && isMatching
    }) match {
      case Fail(error) => Fail(NotCompatibleTypesError(this, graphQLType, error.getMessage))
      case Success() => Success()
    }
  }
  def anySubtypeSatisfies(graphQLType: GraphQLType[_], resolveTrace: GraphResolveTrace): Failable = {
    val f = Fail(NotCompatibleTypesError(this, graphQLType, s"${graphQLType.getTypeKeyword} ${graphQLType.getStringName} has no compatible types in union ${graphQLType.getStringName}")): Failable
    types.foldLeft(f)((acc: Failable, unionTypeValue: GraphQLType[_]) => {
      lazy val isMatching = unionTypeValue.satisfiesType(graphQLType, resolveTrace)
      acc || isMatching
    }) || f
  }
  def anySubtypeIsSatisfiedBy(graphQLType: GraphQLType[_], resolveTrace: GraphResolveTrace): Failable = {
    val f = Fail(NotCompatibleTypesError(this, graphQLType, s"${graphQLType.getTypeKeyword} ${graphQLType.getStringName} has no compatible types in union ${graphQLType.getStringName}")): Failable
    types.foldLeft(f)((acc: Failable, unionTypeValue: GraphQLType[_]) => {
      lazy val isMatching = graphQLType.satisfiesType(unionTypeValue, resolveTrace)
      acc || isMatching
    }) || f
  }

  override def onSatisfactionCheck(graphQLType: GraphQLType[_], resolveTrace: GraphResolveTrace): Failable = graphQLType match {
    case union: GraphQLUnionType => types.foldLeft(Success(): Failable)((acc: Failable, typeValue: GraphQLType[_]) => {
      acc && union.anySubtypeIsSatisfiedBy(typeValue, resolveTrace)
    })
    case refType: GraphQLRefType => onSatisfactionCheck(refType.resolve, resolveTrace)
    case refInterface: GraphQLRefInterface => onSatisfactionCheck(refInterface.resolve, resolveTrace)
    case _ =>
      everySubtypeSatisfies(graphQLType, resolveTrace)
  }

  override def onDirectionExtraction(resolveTrace: GraphResolveTrace): GraphQLTypeDirection = GraphQLDirectionOutput()
  override def onTypeValidation(resolveTrace: GraphResolveTrace): Failable = types.foldLeft(Success(): Failable)((acc: Failable, unionTypeValue: GraphQLType[_]) => {
    acc && unionTypeValue.valdiateType(resolveTrace) && (
      if(unionTypeValue.getDirection(resolveTrace) !~ getDirection(resolveTrace))
        Fail(MixedTypesDirectionError(this, unionTypeValue, s"when adding new type to union"))
      else Success()
    )
  })
}