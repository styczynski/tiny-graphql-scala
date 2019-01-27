package parser.schema.types

import parser.schema.GraphResolveTrace
import parser.exceptions._

final case class GraphQLField(key: String, args: Map[String, GraphQLType[_]], typeValue: Option[GraphQLType[_]]) {
  def withArgs(newArgs: Map[String, GraphQLType[_]]): GraphQLField = copy(args = newArgs)
  def withArg(newArg: String, newType: GraphQLType[_]): GraphQLField = copy(args = args + (newArg -> newType))
  def withType(newType: GraphQLType[_]): GraphQLField = copy(typeValue = Some(newType))
  def getType: GraphQLType[_] = typeValue.get
  def getArgsString: String = {
    val text = args.foldLeft("")((acc, arg) => {
      if (acc.isEmpty) s"${arg._1}: ${arg._2.getStringName}" else s"${arg._1}: ${arg._2.getStringName}, $acc"
    })
    s"($text)"
  }
  def getArgs: Map[String, GraphQLType[_]] = args
  def toString(nestedMode: Boolean, isTop: Boolean): String =
    s"$key$getArgsString: ${typeValue.get.toString(nestedMode, isTop)}"
  override def toString: String = toString(nestedMode=true, isTop=true)
  def validateType(resolveTrace: GraphResolveTrace = new GraphResolveTrace()): Failable = {
    args.foldLeft(Success(): Failable)((acc, argSpec) => {
      acc && (if (argSpec._2.getDirection() ~~ GraphQLDirectionInput()) Success() else Fail(FieldArgInvalidType(argSpec._1, argSpec._2)))
    })
  }
  def hasSatisfableArg(argName: String, typeValue: GraphQLType[_]): Failable = args.get(argName).get.satisfiesType(typeValue) match {
    case Fail(_) => Fail(NotCompatibleTypesError(args.get(argName).get, typeValue, s"Args on field ${key} are not compatible"))
    case Success() => Success()
  }
  def satisfiesTypeByArgs(field: GraphQLField, resolveTrace: GraphResolveTrace = new GraphResolveTrace()): Failable = {
    args.foldLeft(Success(): Failable)((acc, argSpec) => {
      acc && field.hasSatisfableArg(argSpec._1, argSpec._2)
    })
  }
  def satisfiesTypeByReturn(field: GraphQLField, resolveTrace: GraphResolveTrace = new GraphResolveTrace()): Failable = {
    typeValue.get.satisfiesType(field.typeValue.get, resolveTrace)
  }
  def satisfiesType(field: GraphQLField, resolveTrace: GraphResolveTrace = new GraphResolveTrace()): Failable = {
    satisfiesTypeByReturn(field, resolveTrace) && satisfiesTypeByArgs(field, resolveTrace)
  }
}