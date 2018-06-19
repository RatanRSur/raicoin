import scala.collection.SortedSet
import scala.util.{Try, Success, Failure}
import Exceptions._

class Blockchain(val blocks: Seq[Block] = Seq(new RootBlock())) extends Seq[Block] {

  def :+(block: Block): Blockchain = new Blockchain(blocks :+ block)

  // Members declared in IterableLike
  def iterator: Iterator[Block] = blocks.iterator

  // Members declared in SeqLike
  def apply(idx: Int): Block = blocks(idx)
  def length: Int            = blocks.length

  def mineBlock(transactions: Seq[Transaction],
                miner: String,
                newUsers: Seq[String] = Seq.empty): Blockchain = {
    customRequire(transactions.nonEmpty,
                  new IllegalTransactions("No transactions to put in block."))
    this :+ new MinedBlock(this.last, transactions, miner, newUsers)
  }

  override val toString: String = s"${getClass.getName}(\n  ${blocks.mkString("\n  ")}))"

}
