package parser.requests.types

abstract class QueryField[T] {
  def isLeaf: Boolean
  def getNestedFields: List[QueryField[_]]
  def withField(field: QueryField[_]): QueryField[T]
  def withName(newName: Option[String]): QueryField[T]
  def getName: Option[String]
  override def toString: String = s"${getName.get} $getBodyString"
  def getBodyString: String = {
    val contentString = getNestedFields.foldLeft("")((acc, field: QueryField[_]) => {
      val typeText = field.toString.split("\\r?\\n").foldLeft("")((acc, line) => acc + (if (acc.isEmpty) "" else "   ") + line + "\n")
      s"$acc${if (acc.isEmpty) "" else "    "}$typeText"
    })
    s"""|{
        |    $contentString}""".stripMargin
  }
}
