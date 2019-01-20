package parser.exceptions

final case class TypeMissingError(val name: String) extends Error(s"The type $name was not declared.") {
}
