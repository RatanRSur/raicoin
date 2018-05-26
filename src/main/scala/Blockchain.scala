import scala.collection.SortedSet
import scala.util.{Try, Success, Failure}
import Exceptions._

class Blockchain(private val blocks: Seq[Block] = Seq(new RootBlock())) extends Seq[Block] {

  private def :+(block: Block) = new Blockchain(blocks :+ block)

  // Members declared in IterableLike
  def iterator: Iterator[Block] = blocks.iterator

  // Members declared in SeqLike
  def apply(idx: Int): Block = blocks(idx)
  def length: Int            = blocks.length

  def mineBlock(transactions: Seq[Transaction],
                miner: String,
                newUsers: Seq[String] = Seq.empty): Try[Blockchain] = {
    customRequire(transactions.nonEmpty,
                  new IllegalTransactions("No transactions to put in block."))
    Try(new MinedBlock(this.last, transactions, miner, newUsers)).map(block => this :+ block)
  }

}
