package parser.schema

import parser.schema.types.GraphQLScalarType

object DefaultGraphQLSchema {

  final def STRING = GraphQLScalarType(Some("String"))
  final def INT = GraphQLScalarType(Some("Int"))
  final def BOOLEAN = GraphQLScalarType(Some("Boolean"))
  final def ID = GraphQLScalarType(Some("ID"))
  final def FLOAT = GraphQLScalarType(Some("Float"))

  def implement(env: GraphQLSchema): GraphQLSchema = {
    env.registerType(STRING)
      .registerType(INT)
      .registerType(FLOAT)
      .registerType(BOOLEAN)
      .registerType(ID)
  }
}
