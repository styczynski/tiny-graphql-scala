package parser.schema
import parser.schema.types.GraphQLType
import parser.schema.types.{GraphQLTypeDirection, GraphQLDirectionOutput}

final class GraphResolveTrace {
  var directionTrace: Map[Option[String], GraphQLTypeDirection] = Map()
  var validationTrace: Set[Option[String]] = Set()
  var satisfactionTrace: Set[(Option[String], Option[String])] = Set()

  def markDirection(typeValue: GraphQLType[_], graphQLTypeDirection: GraphQLTypeDirection = GraphQLDirectionOutput()): GraphResolveTrace = {
    if(!guardDirection(typeValue)) overrideDirectionCache(typeValue, graphQLTypeDirection)
    this
  }

  def markValidation(typeValue: GraphQLType[_]): GraphResolveTrace = {
    if(!guardValidation(typeValue)) overrideValidationCache(typeValue)
    this
  }

  def markSatisfaction(superTypeValue: GraphQLType[_], subTypeValue: GraphQLType[_]): GraphResolveTrace = {
    if(!guardSatisfaction(superTypeValue, subTypeValue)) overrideSatisfactionCache(superTypeValue, subTypeValue)
    this
  }

  def overrideDirectionCache(typeValue: GraphQLType[_], graphQLTypeDirection: GraphQLTypeDirection): GraphResolveTrace = {
    directionTrace = directionTrace + (typeValue.getName -> graphQLTypeDirection)
    this
  }

  def overrideValidationCache(typeValue: GraphQLType[_]): GraphResolveTrace = {
    validationTrace = validationTrace + typeValue.getName
    this
  }

  def overrideSatisfactionCache(superTypeValue: GraphQLType[_], subTypeValue: GraphQLType[_]): GraphResolveTrace = {
    satisfactionTrace = satisfactionTrace + ((superTypeValue.getName, subTypeValue.getName))
    this
  }

  def getCachedDirection(typeValue: GraphQLType[_]): GraphQLTypeDirection = directionTrace.get(typeValue.getName).get
  def guardDirection(typeValue: GraphQLType[_]): Boolean = directionTrace.get(typeValue.getName).isDefined
  def guardValidation(typeValue: GraphQLType[_]): Boolean = validationTrace(typeValue.getName)
  def guardSatisfaction(superTypeValue: GraphQLType[_], subTypeValue: GraphQLType[_]): Boolean = satisfactionTrace((superTypeValue.getName, subTypeValue.getName))
}
