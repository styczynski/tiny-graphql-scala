package parser.schema.types

import parser.exceptions.{ImplementsNonAbstractTypeError, NotCompatibleTypesError}

abstract class GraphQLType[T](val name: Option[String] = None, val isNullableValue: Boolean = true) {
  def withName(newName: String): GraphQLType[T]
  def isNullable: Boolean = isNullableValue
  def makeCopy: GraphQLType[T]
  def withNullability(shouldBeNullable: Boolean): GraphQLType[T]
  def getInterface: Option[GraphQLComposableType[_]] = None
  def withInterface(typeInterface: Option[GraphQLComposableType[_]]): GraphQLType[T] = makeCopy
  /*def withInterface(typeInterface: Option[GraphQLComposableType[_]]): GraphQLType[T] = typeInterface match {
    case Some(typeInterfaceValue) => makeCopy if (typeInterfaceValue.isAbstract) makeCopy else
      throw ImplementsNonAbstractTypeError(getStringName, typeInterfaceValue.getStringName)
    case None => makeCopy
  }*/
  def getName: Option[String] = name
  def getStringName: String = getName match {
    case Some(nameValue) => nameValue
    case None => toString(nestedMode = false)
  }
  def getTypeKeyword = "type"
  def getModifiersText: String = if (!isNullable) "!" else ""
  def isAbstract: Boolean = false
  def getFormattedString(nestedMode: Boolean, isTop: Boolean): String
  def toString(nestedMode: Boolean = true, isTop: Boolean = true): String = {
    if (!nestedMode && !isTop) getName match {
      case Some(nameText) => s"$nameText$getModifiersText"
      case None => getFormattedString(nestedMode, isTop)
    } else getFormattedString(nestedMode, isTop)
  }
  def validateType: Boolean = true
  def satisfiesType(graphQLType: GraphQLType[_], resolveTrace: Set[(Option[String], Option[String])] = Set()): Boolean = graphQLType match {
    case refType: GraphQLRefType => satisfiesType(refType.resolve, resolveTrace)
    case refInterface: GraphQLRefInterface => satisfiesType(refInterface.resolve, resolveTrace)
    case _ => satisfiesTypeModifiers(graphQLType) && (resolveTrace((getName, graphQLType.getName)) || graphQLType.equals(this))
  }
  def satisfiesTypeModifiers(graphQLType: GraphQLType[_]): Boolean = if(isNullable == graphQLType.isNullable) true
    else throw NotCompatibleTypesError(this, graphQLType, "Nullability does not match")
  override def toString: String = s"$getTypeKeyword ${toString(nestedMode = false)}"

  def ~= (o: Any): Boolean = this ~< o && this ~> o

  def ~< (o: Any): Boolean = o match {
    case typeValue: GraphQLType[_] => satisfiesType(typeValue)
    case _ => false
  }

  def ~> (o: Any): Boolean = o match {
    case typeValue: GraphQLType[_] => typeValue.satisfiesType(this)
    case _ => false
  }
}