package parser.schema.types

import parser.exceptions.{ImplementsNonAbstractTypeError, NotCompatibleTypesError}
import parser.exceptions.{Fail, Failable, Success}
import parser.schema.GraphResolveTrace

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

  protected def onTypeValidation(resolveTrace: GraphResolveTrace): Failable = Success()
  protected def onSatisfactionCheck(graphQLType: GraphQLType[_], resolveTrace: GraphResolveTrace): Failable = {
    lazy val isMatching = if(graphQLType.withNullability(isNullable).equals(this)) Success() else Fail(NotCompatibleTypesError(this, graphQLType, "Types do not equal"))
    graphQLType match {
      case refType: GraphQLRefType => onSatisfactionCheck(refType.resolve, resolveTrace)
      case refInterface: GraphQLRefInterface => onSatisfactionCheck(refInterface.resolve, resolveTrace)
      case union: GraphQLUnionType => union.anySubtypeIsSatisfiedBy(this, resolveTrace)
      case _ => satisfiesTypeModifiers(graphQLType) && isMatching
    }
  }
  protected def onDirectionExtraction(resolveTrace: GraphResolveTrace): GraphQLTypeDirection

  def valdiateType(resolveTrace: GraphResolveTrace = new GraphResolveTrace()): Failable = {
    if(resolveTrace.guardValidation(this)) Success() else onTypeValidation(resolveTrace.markValidation(this))
  }
  def satisfiesType(graphQLType: GraphQLType[_], resolveTrace: GraphResolveTrace = new GraphResolveTrace()): Failable = {
    satisfiesTypeModifiers(graphQLType) && (if(resolveTrace.guardSatisfaction(this, graphQLType)) Success() else onSatisfactionCheck(graphQLType, resolveTrace.markSatisfaction(this, graphQLType)))
  }
  def getDirection(resolveTrace: GraphResolveTrace = new GraphResolveTrace()): GraphQLTypeDirection = {
    val dir = if(resolveTrace.guardDirection(this)) resolveTrace.getCachedDirection(this) else onDirectionExtraction(resolveTrace.markDirection(this))
    resolveTrace.overrideDirectionCache(this, dir)
    dir
  }


  def equivalentType(graphQLType: GraphQLType[_]): Failable = {
    satisfiesType(graphQLType) && graphQLType.satisfiesType(this)
  }
  def satisfiesTypeModifiers(graphQLType: GraphQLType[_]): Failable = if((!isNullable && graphQLType.isNullable) || (isNullable == graphQLType.isNullable)) Success()
    else Fail(NotCompatibleTypesError(this, graphQLType, "Nullability does not match"))

  override def toString: String = s"$getTypeKeyword ${toString(nestedMode = false)}"

  def ~!= (o: Any): Boolean = (!(this ~< o)) || (!(this ~> o))

  def ~= (o: Any): Boolean = this ~< o && this ~> o

  def ~< (o: Any): Boolean = o match {
    case typeValue: GraphQLType[_] => satisfiesType(typeValue).hasNotFailed
    case _ => false
  }

  def ~> (o: Any): Boolean = o match {
    case typeValue: GraphQLType[_] => typeValue.satisfiesType(this).hasNotFailed
    case _ => false
  }
}