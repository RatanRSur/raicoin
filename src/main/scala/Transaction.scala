package raicoin

import java.security.MessageDigest

import akka.util.ByteString
import raicoin.Exceptions._
import scorex.crypto.signatures._
import Serializer._
import Serializer.RaicoinJsonProtocols._

case class SignedTransaction(signature: ByteString, transaction: Transaction) {
  def verify: Boolean =
    Curve25519.verify(Signature(signature.toArray), serialize(transaction), transaction.sender)

}

case class Transaction(sender: PublicKey, recipient: PublicKey, amount: Int) extends SHAHashable {
  customRequire(sender != recipient, new IllegalTransactions("sender cannot also be recipient"))
  import HashImplicits._
  @transient val ticks = scala.compat.Platform.currentTime

  @transient val hash = {
    val sha = MessageDigest.getInstance("SHA-256")
    Seq(sender, recipient, amount.hash).foreach(sha.update)
    sha.digest
  }
  override def equals(that: Any): Boolean = {
    that match {
      case that: Transaction =>
        ticks == that.ticks && sender.deep == that.sender.deep && recipient.deep == that.recipient.deep && amount == that.amount
      case _ => false
    }
  }

  def sign(privateKey: PrivateKey): SignedTransaction = {
    SignedTransaction(ByteString(Curve25519.sign(privateKey, serialize(this))), this)
  }
}
