package parser.exceptions

sealed abstract class Failable {
  def hasFailed: Boolean
  def hasNotFailed: Boolean = !hasFailed
  def hasNotFailedOrThrow: Boolean = hasNotFailed
  def && (f: => Failable): Failable = if(hasFailed) this else f
  def || (f: => Failable): Failable = if(hasFailed)
    if (f.hasFailed) f else Success()
    else Success()
  def !(): Failable = if(hasFailed) Success() else this
}

final case class Fail(error: Error) extends Failable {
  def hasFailed: Boolean = true
  def getError: Error = error
  def getMessage: String = error.getMessage
  override def hasNotFailedOrThrow: Boolean = throw getError
}

final case class Success() extends Failable {
  def hasFailed: Boolean = false
}