package parser.requests.types

final case class QueryFieldBlock(name: Option[String] = None, fields: List[QueryField[_]] = List()) extends QueryField[QueryFieldBlock] {
  override def isLeaf: Boolean = true
  override def getNestedFields: List[QueryField[_]] = fields
  override def withField(field: QueryField[_]): QueryFieldBlock = copy(fields = field :: fields)
  override def withName(newName: Option[String]): QueryFieldBlock = copy(name = newName)
  override def getName: Option[String] = name
}
