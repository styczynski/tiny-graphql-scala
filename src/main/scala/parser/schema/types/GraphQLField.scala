package parser.schema.types

final case class GraphQLField[T](args: List[String], typeValue: GraphQLType[T]) {
  def withArgs(newArgs: List[String]): GraphQLField[T] = copy(args = newArgs)
  def withType(newType: GraphQLType[T]): GraphQLField[T] = copy(typeValue = newType)
  def getType: GraphQLType[_] = typeValue
  def toString(nestedMode: Boolean, isTop: Boolean): String = typeValue.toString(nestedMode, isTop)
  override def toString: String = toString(nestedMode=true, isTop=true)
}