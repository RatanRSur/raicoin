package raicoin

import java.io.File
import org.apache.commons.io.FileUtils
import org.apache.commons.codec.binary.Hex
import scorex.crypto.signatures._

import Serializer._
import Serializer.RaicoinJsonProtocols._

object Blockchain {
  def fromFile(blockchainFile: File): Blockchain = {
    deserialize[Blockchain](FileUtils.readFileToByteArray(blockchainFile))
  }
}

case class Blockchain(blocksByHash: Map[String, Block] = Map(
                        (Hex.encodeHexString(EmptyRootBlock.hash) ->
                          EmptyRootBlock)),
                      tips: Seq[Block] = Seq(EmptyRootBlock),
                      val difficulty: Int = 2)
    extends Iterable[Block]
    with Serializable {

  def tip             = tips.head
  def height          = tips.map(_.index).max + 1
  def apply(idx: Int) = iterator.drop(idx).next()
  def ledger          = tip.ledger

  @transient override val toString: String = {
    val iter = iterator
    s"${getClass.getName} main line:\n  ${iter.take(5).mkString("\n  ")}${if (iter.hasNext) "\n  ..."
    else ""}"
  }

  def append(minedBlock: MinedBlock): Blockchain = {
    val parentBlock = blocksByHash.get(minedBlock.parentHash)
    if (parentBlock.isDefined) {
      val updatedBlocksByHash = blocksByHash + (Hex.encodeHexString(minedBlock.hash) -> minedBlock)
      val updatedTips = {
        val replaceIndex =
          tips.indexWhere(x => Hex.encodeHexString(x.hash) == minedBlock.parentHash)
        if (replaceIndex == -1) {
          tips :+ minedBlock
        } else {
          tips.patch(replaceIndex, Seq(minedBlock), 1)
        }
      }

      new Blockchain(updatedBlocksByHash, updatedTips, difficulty)
    } else {
      this
    }
  }

  def containsParentOf(mb: MinedBlock): Boolean = {
    blocksByHash.contains(mb.parentHash)
  }

  def resolveOrphans(blocks: Seq[MinedBlock]): (Blockchain, Seq[MinedBlock]) = {
    val notOrphans = blocks.filter(this.containsParentOf)
    if (notOrphans.isEmpty) {
      (this, blocks)
    } else {
      val orphans = blocks.filterNot(this.containsParentOf)
      val updatedChain = (this /: notOrphans) {
        case (chain, block) => chain.append(block)
      }
      updatedChain.resolveOrphans(orphans)
    }
  }

  def iterator =
    new Iterator[Block] {
      var current    = tip
      var rootUnread = true
      def hasNext    = rootUnread
      def next() = {
        val ret = current
        current = current match {
          case mb: MinedBlock => blocksByHash(mb.parentHash)
          case rb: RootBlock => {
            rootUnread = false
            current
          }
        }
        ret
      }
    }.toSeq.reverseIterator

  def mineBlock(signedTransactions: Seq[SignedTransaction], miner: PublicKey): Blockchain = {
    append(
      new UnminedBlock(Hex.encodeHexString(tip.hash),
                       tip.index,
                       ledger,
                       signedTransactions,
                       miner,
                       difficulty).mine)
  }

  def mineBlock(signedTransaction: SignedTransaction, miner: PublicKey): Blockchain =
    mineBlock(Seq(signedTransaction), miner)

}
