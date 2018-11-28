package raicoin

import scala.collection.SortedSet
import scala.util.{Try, Success, Failure}
import Exceptions._
import org.apache.commons.codec.binary.Hex
import scorex.crypto.signatures._

class Blockchain(blocksByHash: Map[String, Block] = Map((Hex.encodeHexString(EmptyRootBlock.hash) ->
                                                         EmptyRootBlock)),
                 tips: Seq[Block]                 = Seq(EmptyRootBlock))
    extends Iterable[Block] with Serializable {

  val tip             = tips.head
  val difficulty      = 1
  val height          = tips.map(_.index).max + 1
  def apply(idx: Int) = iterator.drop(idx).next()
  val ledger          = tip.ledger

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

      new Blockchain(updatedBlocksByHash, updatedTips)
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

  def mineBlock(signedTransactions: Seq[SignedTransaction],
                miner: PublicKey,
                newPublicKeys: Seq[PublicKey] = Seq.empty): Blockchain = {
    append(
      new MinedBlock(Hex.encodeHexString(tip.hash),
                     tip.index,
                     ledger,
                     signedTransactions,
                     miner,
                     newPublicKeys,
                     difficulty))
  }

  override val toString: String =
    s"${getClass.getName} main line:\n  ${iterator.take(5).mkString("\n  ")})..."

}
