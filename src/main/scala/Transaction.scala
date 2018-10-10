package raicoin

import Exceptions._

import akka.util.ByteString
import scorex.crypto._
import scorex.crypto.signatures._
import org.apache.commons.lang3.SerializationUtils.serialize

object Transaction {
  def sign(privateKey: PrivateKey, transaction: Transaction): SignedTransaction = {
    SignedTransaction(
      ByteString(Curve25519.sign(privateKey, serialize(transaction))),
      transaction)
  }

  def verify(signedTransaction: SignedTransaction, publicKey: PublicKey): Boolean = {
    Curve25519.verify(Signature(signedTransaction.signature.toArray),
      serialize(signedTransaction.transaction), publicKey: PublicKey)
  }

}

case class SignedTransaction(signature: ByteString, transaction: Transaction)

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
}
