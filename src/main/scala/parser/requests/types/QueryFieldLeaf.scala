package parser.requests.types

final case class QueryFieldLeaf(name: Option[String] = None) extends QueryField[QueryFieldLeaf] {
  override def isLeaf: Boolean = true
  override def getNestedFields: List[QueryField[_]] = List()
  override def withField(field: QueryField[_]): QueryFieldLeaf = copy(name = name)
  override def withName(newName: Option[String]): QueryFieldLeaf = copy(name = newName)
  override def getName: Option[String] = name
  override def getBodyString: String = ""
}
