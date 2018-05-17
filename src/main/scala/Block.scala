abstract class Block {
  val ledger: Ledger
}

object RootBlock extends Block {
  val ledger = new Ledger(Map.empty)
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

  val isValid = transactions.forall { t => ledger(t.sender).balance >= 0 }
}
