import scala.util.Random
import scala.collection.SortedSet
import java.security.MessageDigest

abstract class Block extends SHAHashable {
  val ledger: Ledger
  val isValid: Boolean
}

class RootBlock(val ledger: Ledger = new Ledger()) extends Block {
  val isValid            = true
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

  val isValid =
    ledger.isValid &&
      transactions.forall(_.isValid) &&
      transactions.nonEmpty

  val timestamp = java.util.Calendar.getInstance.getTimeInMillis
  val hashDependencies =
    Seq[SHAHashable](previousBlock, transactions, ledger, timestamp).map(_.hash)

}
