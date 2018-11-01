package raicoin

import scala.collection.SortedSet
import scala.util.{Try, Success, Failure}
import Exceptions._
import org.apache.commons.codec.binary.Hex
import scorex.crypto.signatures._

class BlockWithParent(val block: Block, val parent: Option[BlockWithParent]) extends Serializable {
  val index: Int = parent.map(_.index + 1).getOrElse(0)
}

class Blockchain(blocksByHash: Map[String, BlockWithParent] = {
  Map((Hex.encodeHexString(EmptyRootBlock.hash) -> new BlockWithParent(EmptyRootBlock, None)))
}, tips: Seq[BlockWithParent] = Seq(new BlockWithParent(EmptyRootBlock, None)))
    extends Iterable[Block] with Serializable {

  val tip             = tips.head
  val difficulty      = 1
  val height          = tips.map(_.index).max + 1
  def apply(idx: Int) = iterator.drop(idx).next()
  val ledger          = tip.block.ledger

  def append(minedBlock: MinedBlock): Blockchain = {
    val parentBlock = blocksByHash.get(minedBlock.parentHash)
    if (parentBlock.isDefined) {
      val wrappedBlock        = new BlockWithParent(minedBlock, parentBlock)
      val updatedBlocksByHash = blocksByHash + (Hex.encodeHexString(minedBlock.hash) -> wrappedBlock)
      val updatedTips = {
        val replaceIndex =
          tips.indexWhere(x => Hex.encodeHexString(x.block.hash) == minedBlock.parentHash)
        if (replaceIndex == -1) {
          tips :+ wrappedBlock
        } else {
          tips.patch(replaceIndex, Seq(wrappedBlock), 1)
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
        val ret = current.block
        current = current.parent match {
          case Some(mb) => mb
          case None => {
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
    customRequire(signedTransactions.nonEmpty,
                  new IllegalTransactions("No transactions to put in block."))
    append(
      new MinedBlock(Hex.encodeHexString(tip.block.hash),
                     ledger,
                     signedTransactions,
                     miner,
                     newPublicKeys,
                     difficulty))
  }

  override val toString: String =
    s"${getClass.getName} main line:\n  ${iterator.mkString("\n  ")})"

}
