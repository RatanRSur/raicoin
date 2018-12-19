package raicoin

object Exceptions {
  def customRequire(cond: Boolean, e: Exception) = if (!cond) throw e else ()
  class IllegalTransactions(val message: String) extends Exception(message)
}
