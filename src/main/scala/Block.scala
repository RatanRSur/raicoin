abstract class Block {
  val ledger: Ledger
  val isValid: Boolean
}

object RootBlock extends Block {
  val ledger = new Ledger(Map.empty)
  val isValid = true
}

class MinedBlock(
  val prevBlock: Block,
  val transactions: Seq[Transaction],
  val miner: String,
  val newUsers: Set[String]) extends Block {

  val ledger = prevBlock.ledger
    .addUsers(newUsers)
    .rewardMiner(miner)
    .applyTransactions(transactions)

  val isValid =
    ledger.isValid &&
    transactions.forall(_.isValid) &&
    transactions.nonEmpty
}
