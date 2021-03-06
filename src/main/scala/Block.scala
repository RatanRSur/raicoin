package raicoin

import java.nio.ByteBuffer
import java.security.MessageDigest

import org.apache.commons.codec.binary.Hex
import scorex.crypto.signatures._

object BlockUtils {
  def isCorrect(difficulty: Int, candidate: Array[Byte]): Boolean =
    candidate.startsWith(Seq.fill(difficulty)(0))

  def hashWithNonce(shaInstance: MessageDigest,
                    hashDependencies: Seq[SHAHashable],
                    n: Int): Array[Byte] = {
    shaInstance.reset()
    hashDependencies.map(_.hash).foreach(shaInstance.update)
    val buf = ByteBuffer.allocate(4).putInt(n)
    shaInstance.update(buf.array)
    shaInstance.digest
  }
}

abstract class Block extends SHAHashable with Serializable {
  val index: Int
  val ledger: Ledger
}

case class RootBlock(ledger: Ledger = new Ledger()) extends Block {
  @transient val hash = {
    val sha = MessageDigest.getInstance("SHA-256")
    sha.update(ledger.hash)
    sha.digest
  }
  @transient val index = 0
  @transient override val toString: String =
    s"${getClass.getName}(hash: ${Hex.encodeHexString(hash).take(6)}...)"
}

object EmptyRootBlock extends RootBlock()

case class UnminedBlock(parentHash: String,
                        parentIndex: Int,
                        parentLedger: Ledger,
                        signedTransactions: Seq[SignedTransaction],
                        miner: PublicKey,
                        difficulty: Int) {
  import HashImplicits._

  val transactions = signedTransactions.map(_.transaction)
  val ledger = parentLedger
    .rewardMiner(miner)
    .applyTransactions(transactions)
    .get

  import BlockUtils._
  val sha              = MessageDigest.getInstance("SHA-256")
  val hashDependencies = Seq[SHAHashable](parentHash, ledger, transactions)

  private var nonce = 0
  def mine: MinedBlock = {
    while (!isCorrect(difficulty, hashWithNonce(sha, hashDependencies, nonce))) {
      nonce += 1
    }

    MinedBlock(parentHash, parentIndex + 1, ledger, signedTransactions, difficulty, nonce)
  }
}

case class MinedBlock(parentHash: String,
                      index: Int,
                      ledger: Ledger,
                      signedTransactions: Seq[SignedTransaction],
                      difficulty: Int,
                      nonce: Int)
    extends Block {

  def transactions = signedTransactions.map(_.transaction)
  import BlockUtils._
  import HashImplicits._
  def hashDependencies =
    Seq[SHAHashable](parentHash, ledger, transactions)

  @transient val hash: Array[Byte] =
    hashWithNonce(MessageDigest.getInstance("SHA-256"), hashDependencies, nonce)

  override def equals(that: Any): Boolean = {
    that match {
      case that: MinedBlock =>
        transactions.zip(that.transactions).forall { case (x, y) => x == y } &&
          ledger == that.ledger &&
          parentHash == that.parentHash &&
          hash.deep == that.hash.deep
      case _ => false
    }
  }

  assume(isCorrect(difficulty, hash))

  override def toString: String =
    s"${getClass.getName}(hash: ${Hex.encodeHexString(hash).take(6)}..., parentHash: ${parentHash.take(6)}...)"

}
