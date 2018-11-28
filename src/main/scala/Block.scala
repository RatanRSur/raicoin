package raicoin

import java.nio.ByteBuffer
import org.apache.commons.codec.binary.Hex
import scorex.crypto.signatures._

abstract class Block extends SHAHashable with Serializable {
  val index: Int
  val ledger: Ledger
}

case class RootBlock(val ledger: Ledger = new Ledger()) extends Block {
  val hashDependencies = Seq(ledger.hash)
  val index = 0
  override val toString: String =
    s"${getClass.getName}(hash: ${Hex.encodeHexString(hash).take(6)}...)"
}

object EmptyRootBlock extends RootBlock()

case class MinedBlock(val parentHash: String,
                      parentIndex: Int,
                      parentLedger: Ledger,
                      signedTransactions: Seq[SignedTransaction],
                      miner: PublicKey,
                      newPublicKeys: Seq[PublicKey],
                      difficulty: Int,
                      checkNonce: Option[Int] = None)
    extends Block {

  val index = parentIndex + 1
  val transactions = signedTransactions.map(_.transaction)

  val ledger = parentLedger
    .addPublicKeys(newPublicKeys)
    .rewardMiner(miner)
    .applyTransactions(transactions)
    .get

  val hashDependencies = Seq[SHAHashable](parentHash, transactions, ledger).map(_.hash)

  private def hashWithNonce(n: Int): Array[Byte] = {
    sha.reset()
    hashDependencies.foreach(sha.update)
    val buf = ByteBuffer.allocate(4).putInt(n)
    sha.update(buf.array)
    sha.digest
  }

  private def isCorrect(candidate: Array[Byte]): Boolean =
    candidate.startsWith(Seq.fill(difficulty)(0))

  private var nonce = 0 //this is only to be modified by the loop in hash (below)
  override lazy val hash = {
    checkNonce match {
      case Some(n) => require(isCorrect(hashWithNonce(n)), "Invalid hash")
      case None => {
        while (!isCorrect(hashWithNonce(nonce))) {
          nonce += 1
        }
      }
    }
    hashWithNonce(nonce)
  }

  override lazy val toString: String =
    s"${getClass.getName}(hash: ${Hex.encodeHexString(hash).take(6)}..., parentHash: ${parentHash.take(6)}...)"

}
