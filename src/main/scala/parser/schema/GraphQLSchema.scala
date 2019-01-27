package parser.schema

import parser.exceptions.{AnonymousTypeForbiddenError, InvalidTypeSpecificationError, TypeMissingError}
import parser.schema.types.{GraphQLInterface, GraphQLInterfaceType, GraphQLType}

final class GraphQLSchema(var types: Map[String, GraphQLType[_]] = Map(), var interfaces: Map[String, GraphQLInterfaceType] = Map()) {
  def findType(name: String): GraphQLType[_] = {
    types.get(name) match {
      case Some(typeObj) => typeObj
      case None => interfaces.get(name) match {
        case Some(typeObj) => typeObj
        case None => throw TypeMissingError(name)
      }
    }
  }

  def findInterface(name: String): GraphQLInterface = {
    interfaces.get(name) match {
      case Some(typeObj) => typeObj
      case None => throw TypeMissingError(name)
    }
  }

  def validate: Boolean = types.forall(_._2.valdiateType().hasNotFailedOrThrow) && interfaces.forall(_._2.valdiateType().hasNotFailedOrThrow)

  def registerType(graphQLType: GraphQLType[_]): GraphQLSchema = {
    graphQLType.getName match {
      case Some(name) => graphQLType match {
        case interfaceType: GraphQLInterfaceType => interfaces = interfaces + (name -> interfaceType); this
        case _ => types = types + (name -> graphQLType); this
      }
      case None => throw AnonymousTypeForbiddenError("Could not declare anonymous type in global scope")
    }
  }

  override def toString: String = {
    val typesString = types.foldLeft("")((acc: String, keyValue: (String, GraphQLType[_])) => {
      val typeText = keyValue._2.toString(nestedMode = false).split("\\r?\\n").foldLeft("")((acc, line) => acc + (if (acc.isEmpty) "" else "    ") + line + "\n")
      s"$acc    ${keyValue._2.getTypeKeyword} $typeText"
    })
    val interfacesString = interfaces.foldLeft("")((acc: String, keyValue: (String, GraphQLInterfaceType)) => {
      val typeText = keyValue._2.toString(nestedMode = false).split("\\r?\\n").foldLeft("")((acc, line) => acc + (if (acc.isEmpty) "" else "    ") + line + "\n")
      s"$acc    ${keyValue._2.getTypeKeyword} $typeText"
    })
    s"env {\n$interfacesString$typesString}"
  }
}
