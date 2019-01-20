package parser.exceptions

final case class AnonymousTypeForbiddenError(override val message: String) extends Error(message) {
}
