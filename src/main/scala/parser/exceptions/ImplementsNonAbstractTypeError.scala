package parser.exceptions

final case class ImplementsNonAbstractTypeError(typeName: String, interfaceName: String) extends Error(s"Type $typeName cannot implement non abstract type $interfaceName") {
}

