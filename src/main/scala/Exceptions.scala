package raicoin

import scorex.crypto.signatures._

object Exceptions {
  def customRequire(cond: Boolean, e: Exception) = if (!cond) throw e else ()
  class IllegalTransactions(val message: String) extends Exception(message)
  class PublicKeyAlreadyExists(val pk: PublicKey)   extends Exception(pk.toString)
}
