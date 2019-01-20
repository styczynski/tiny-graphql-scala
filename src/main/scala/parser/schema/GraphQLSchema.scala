package parser.schema

import parser.exceptions.{AnonymousTypeForbiddenError, InvalidTypeSpecificationError, TypeMissingError}
import parser.schema.types.{GraphQLInterfaceType, GraphQLType}

final case class GraphQLSchema(types: Map[String, GraphQLType[_]] = Map(), interfaces: Map[String, GraphQLInterfaceType] = Map()) {
  def findType(name: String): GraphQLType[_] = {
    types.get(name) match {
      case Some(typeObj) => typeObj
      case None => interfaces.get(name) match {
        case Some(typeObj) => typeObj
        case None => throw TypeMissingError(name)
      }
    }
  }

  def findInterface(name: String): GraphQLInterfaceType = {
    interfaces.get(name) match {
      case Some(typeObj) => typeObj
      case None => throw TypeMissingError(name)
    }
  }

  def registerType(graphQLType: GraphQLType[_]): GraphQLSchema = {
    if (graphQLType.validateType)
    graphQLType.getName match {
      case Some(name) => graphQLType match {
        case interfaceType: GraphQLInterfaceType => copy(interfaces = interfaces + (name -> interfaceType))
        case _ => copy(types = types + (name -> graphQLType))
      }
      case None => throw AnonymousTypeForbiddenError("Could not declare anonymous type in global scope")
    }
    else throw InvalidTypeSpecificationError(graphQLType, "Validation method returns false")
  }

  override def toString: String = {
    val typesString = types.foldLeft("")((acc: String, keyValue: (String, GraphQLType[_])) => {
      val typeText = keyValue._2.toString(nestedMode = false).split("\\r?\\n").foldLeft("")((acc, line) => acc + (if (acc.isEmpty) "" else "    ") + line + "\n")
      s"$acc    ${keyValue._2.getTypeKeyword} $typeText"
    })
    val interfacesString = interfaces.foldLeft("")((acc: String, keyValue: (String, GraphQLType[GraphQLInterfaceType])) => {
      val typeText = keyValue._2.toString(nestedMode = false).split("\\r?\\n").foldLeft("")((acc, line) => acc + (if (acc.isEmpty) "" else "    ") + line + "\n")
      s"$acc    ${keyValue._2.getTypeKeyword} $typeText"
    })
    s"env {\n$interfacesString$typesString}"
  }
}
