import scala.util.Random
import scala.collection.SortedSet
import java.security.MessageDigest
import scala.util.{Try, Success, Failure}

abstract class Block extends SHAHashable {
  val ledger: Ledger
}

class RootBlock(val ledger: Ledger = new Ledger()) extends Block {
  val hashDependencies   = Seq()
  override lazy val hash = Array.fill(32)((Random.nextInt(256) - 128).toByte)
}

class MinedBlock(val previousBlock: Block,
                 val transactions: Seq[Transaction],
                 val miner: String,
                 val newUsers: Seq[String])
    extends Block {

  val ledger =
    previousBlock.ledger
      .addUsers(newUsers)
      .rewardMiner(miner)
      .applyTransactions(transactions)
      .get

  val timestamp = java.util.Calendar.getInstance.getTimeInMillis
  val hashDependencies =
    Seq[SHAHashable](previousBlock, transactions, ledger, timestamp).map(_.hash)

}
