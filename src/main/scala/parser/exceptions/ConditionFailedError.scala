package parser.exceptions

final case class ConditionFailedError() extends Error("Logical condition has failed") {
}
