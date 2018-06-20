import scala.util.Random
import scala.collection.SortedSet
import java.security.MessageDigest
import scala.util.{Try, Success, Failure}

object BlockOrdering extends Ordering[Block] {
  def compare(a: Block, b: Block) = a.index.compare(b.index)
}

abstract class Block extends SHAHashable {
  val index: Int
  val ledger: Ledger
  override lazy val toString: String = s"${getClass.getName}(index: $index, hash: $hashHex)"
}

case class RootBlock(val ledger: Ledger = new Ledger()) extends Block {
  val index            = 0
  val hashDependencies = Seq(ledger.hash)
}

case class MinedBlock(val previousBlock: Block,
                      val transactions: Seq[Transaction],
                      val miner: String,
                      val newUsers: Seq[String],
                      val timestamp: Long = java.util.Calendar.getInstance.getTimeInMillis)
    extends Block {
  val index = previousBlock.index + 1

  val ledger =
    previousBlock.ledger
      .addUsers(newUsers)
      .rewardMiner(miner)
      .applyTransactions(transactions)
      .get

  val hashDependencies =
    Seq[SHAHashable](previousBlock, transactions, ledger, timestamp).map(_.hash)

}
