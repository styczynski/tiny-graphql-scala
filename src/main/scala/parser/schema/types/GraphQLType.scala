package parser.schema.types

abstract class GraphQLType[T](val name: Option[String] = None, val isNullableValue: Boolean = true) {
  def withName(newName: String): GraphQLType[T]
  def isNullable: Boolean = isNullableValue
  def makeCopy: GraphQLType[T]
  def withNullability(shouldBeNullable: Boolean): GraphQLType[T]
  def getInterface: Option[GraphQLComposableType[_]] = None
  def withInterface(typeInterface: Option[GraphQLComposableType[_]]): GraphQLType[T] = typeInterface match {
    case Some(typeInterfaceValue) => if (typeInterfaceValue.isAbstract) makeCopy else
      throw new Exception(s"Type $getName cannot implement non abstract type ${typeInterfaceValue.getName}")
    case None => makeCopy
  }
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
  def satisfiesType(graphQLType: GraphQLType[_]): Boolean
  def satisfiesTypeModifiers(graphQLType: GraphQLType[_]): Boolean = if(isNullable == graphQLType.isNullable) true
    else throw new Exception(s"$getTypeKeyword $getStringName does not satisfy ${graphQLType.getTypeKeyword} ${graphQLType.getStringName}: Nullability does not match")
  override def toString: String = s"$getTypeKeyword ${toString(nestedMode = false)}"
}