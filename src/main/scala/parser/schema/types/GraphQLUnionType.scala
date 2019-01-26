package parser.schema.types

import parser.exceptions.NotCompatibleTypesError
import parser.exceptions.{Fail, Success, Failable}

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
  def everySubtypeSatisfies(graphQLType: GraphQLType[_], resolveTrace: Set[(Option[String], Option[String])]): Failable = {
    types.foldLeft(Success(): Failable)((acc: Failable, typeValue: GraphQLType[_]) => {
      lazy val isMatching = typeValue.satisfiesType(graphQLType, resolveTrace)
      acc && isMatching
    }) match {
      case Fail(error) => Fail(NotCompatibleTypesError(this, graphQLType, error.getMessage))
      case Success() => Success()
    }
  }
  def anySubtypeSatisfies(graphQLType: GraphQLType[_], resolveTrace: Set[(Option[String], Option[String])]): Failable = {
    types.foldLeft(Fail(NotCompatibleTypesError(this, graphQLType, s"Type ${graphQLType.getStringName} has no compatible types in union ${graphQLType.getStringName}")): Failable)((acc: Failable, unionTypeValue: GraphQLType[_]) => {
      lazy val isMatching = graphQLType.satisfiesType(unionTypeValue, resolveTrace)
      acc || isMatching
    })
  }
  def anySubtypeIsSatisfiedBy(graphQLType: GraphQLType[_], resolveTrace: Set[(Option[String], Option[String])]): Failable = {
    types.foldLeft(Fail(NotCompatibleTypesError(this, graphQLType, s"Type ${graphQLType.getStringName} has no compatible types in union ${graphQLType.getStringName}")): Failable)((acc: Failable, unionTypeValue: GraphQLType[_]) => {
      lazy val isMatching = unionTypeValue.satisfiesType(graphQLType, resolveTrace)
      acc || isMatching
    })
  }
  override def satisfiesType(graphQLType: GraphQLType[_], resolveTrace: Set[(Option[String], Option[String])]): Failable = {
    val isInTrace = resolveTrace((getName, graphQLType.getName))
    lazy val isMatching = graphQLType match {
      case union: GraphQLUnionType => types.foldLeft(Success(): Failable)((acc: Failable, typeValue: GraphQLType[_]) => {
        acc && union.anySubtypeIsSatisfiedBy(typeValue, resolveTrace + ((getName, graphQLType.getName)))
      })
      case refType: GraphQLRefType => satisfiesType(refType.resolve, resolveTrace)
      case refInterface: GraphQLRefInterface => satisfiesType(refInterface.resolve, resolveTrace)
      case _ =>
        everySubtypeSatisfies(graphQLType, resolveTrace + ((getName, graphQLType.getName)))
    }
    satisfiesTypeModifiers(graphQLType) && (if (isInTrace) Success() else isMatching)
  }
}