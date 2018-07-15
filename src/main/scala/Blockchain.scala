package ratan.blockchain

import scala.collection.SortedSet
import scala.util.{Try, Success, Failure}
import Exceptions._

class Blockchain(tips: Seq[Block] = Seq(new RootBlock()), val difficulty: Int = 1)
    extends Iterable[Block] {

  def this(tip: Block) = this(Seq(tip))
  val sortedTips = tips.sorted(BlockOrdering)
  val tip        = tips.head
  def append(newBlock: MinedBlock): Blockchain = {
    val replaceIndex = tips.indexWhere(_ == newBlock.previousBlock)
    new Blockchain(if (replaceIndex == -1) {
      tips :+ newBlock
    } else {
      tips.patch(replaceIndex, Seq(newBlock), 1)
    })
  }

  def iterator =
    new Iterator[Block] {
      var current    = tip
      var rootUnread = true
      def hasNext    = rootUnread
      def next() = {
        val ret = current
        current = current match {
          case mb: MinedBlock => mb.previousBlock
          case rb: RootBlock => {
            rootUnread = false
            rb
          }
        }
        ret
      }
    }.toSeq.reverseIterator

  val height: Int = tip.index + 1
  def apply(idx: Int): Block = {
    require(idx < height)
    iterator.drop(idx).next()
  }

  def mineBlock(transactions: Seq[Transaction],
                miner: String,
                newUsers: Seq[String] = Seq.empty): Blockchain = {
    customRequire(transactions.nonEmpty,
                  new IllegalTransactions("No transactions to put in block."))
    append(new MinedBlock(this.tip, transactions, miner, newUsers, difficulty))
  }

  override val toString: String =
    s"${getClass.getName} main line:\n  ${iterator.mkString("\n  ")})"

}
