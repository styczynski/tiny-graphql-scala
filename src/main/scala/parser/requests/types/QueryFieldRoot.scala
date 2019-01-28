package parser.requests.types

final case class QueryFieldRoot(fields: List[QueryField[_]] = List()) extends QueryField[QueryFieldRoot] {
  override def isLeaf: Boolean = true
  override def getNestedFields: List[QueryField[_]] = fields
  override def withField(field: QueryField[_]): QueryFieldRoot = copy(fields = field :: fields)
  override def withName(newName: Option[String]): QueryFieldRoot = this
  override def getName: Option[String] = Some("query")
}
