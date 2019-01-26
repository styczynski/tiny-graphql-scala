package parser.schema.types

import parser.exceptions.ImplementsNonAbstractTypeError
import parser.exceptions.{Fail, Failable, Success}

final case class GraphQLCompositeType(override val name: Option[String] = None, override val isNullableValue: Boolean = true, fields: Map[String, GraphQLField[_]] = Map(), typeInterface: Option[GraphQLComposableType[_]] = None) extends GraphQLComposableType[GraphQLCompositeType] {
  override def makeCopy: GraphQLCompositeType = copy()
  override def withNullability(shouldBeNullable: Boolean): GraphQLCompositeType = copy(isNullableValue = shouldBeNullable)
  override def withField(fieldName: String, field: GraphQLField[_]): GraphQLCompositeType = copy(fields = fields + (fieldName -> field))
  override def withName(newName: String): GraphQLCompositeType = copy(name = Some(newName))
  /*override def withInterface(newTypeInterface: Option[GraphQLComposableType[_]]): GraphQLCompositeType = newTypeInterface match {
    case Some(newTypeInterfaceValue) => if(newTypeInterfaceValue.isAbstract) copy(typeInterface = newTypeInterface) else
      throw ImplementsNonAbstractTypeError(getStringName, newTypeInterfaceValue.getStringName)
    case None => copy(typeInterface = newTypeInterface)
  }*/
  override def withInterface(newTypeInterface: Option[GraphQLComposableType[_]]): GraphQLCompositeType =  copy(typeInterface = newTypeInterface)
  override def getInterface: Option[GraphQLComposableType[_]] = typeInterface
  override def getFields: Map[String, GraphQLField[_]] = fields
  override def validateType: Failable = typeInterface match {
    case Some(typeInterfaceValue) => satisfiesType(typeInterfaceValue)
    case None => Success()
  }
}
