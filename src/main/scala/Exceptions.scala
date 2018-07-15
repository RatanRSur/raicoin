package ratan.blockchain

object Exceptions {
  def customRequire(cond: Boolean, e: Exception) = if (!cond) throw e else ()
  class IllegalTransactions(val message: String) extends Exception(message)
  class UserAlreadyExists(val message: String)   extends Exception(message)
}
