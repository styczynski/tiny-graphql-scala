package parser.schema.types

final case class GraphQLField(key: String, args: List[String], typeValue: Option[GraphQLType[_]]) {
  def withArgs(newArgs: List[String]): GraphQLField = copy(args = newArgs)
  def withArg(newArg: String): GraphQLField = copy(args = newArg :: args)
  def withType(newType: GraphQLType[_]): GraphQLField = copy(typeValue = Some(newType))
  def getType: GraphQLType[_] = typeValue.get
  def getArgsString: String = {
    val text = args.foldLeft("")((acc, arg) => {
      if (acc.isEmpty) arg else s"$arg, $acc"
    })
    s"($text)"
  }
  def toString(nestedMode: Boolean, isTop: Boolean): String =
    s"$key$getArgsString: ${typeValue.get.toString(nestedMode, isTop)}"
  override def toString: String = toString(nestedMode=true, isTop=true)
}