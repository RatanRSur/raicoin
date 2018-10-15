package raicoin

import Exceptions._

import akka.util.ByteString
import scorex.crypto._
import scorex.crypto.signatures._
import org.apache.commons.lang3.SerializationUtils.serialize

case class SignedTransaction(signature: ByteString, transaction: Transaction) {
  def verify: Boolean = Curve25519.verify(
    Signature(signature.toArray),
    serialize(transaction),
    transaction.sender)

}

case class Transaction(sender: PublicKey, recipient: PublicKey, amount: Int)
    extends SHAHashable {
  customRequire(sender != recipient, new IllegalTransactions("sender cannot also be recipient"))
  val hashDependencies = Seq(sender, recipient, amount.hash)
  override def equals(that: Any): Boolean = {
    that match {
      case that: Transaction => sender.deep == that.sender.deep && recipient.deep == that.recipient.deep && amount == that.amount
      case _ => false
    }
  }

  def sign(privateKey: PrivateKey): SignedTransaction = {
    SignedTransaction(
      ByteString(Curve25519.sign(privateKey, serialize(this))),
      this)
  }
}
