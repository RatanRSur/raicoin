class Ledger(
  private val ledger: Map[String, User]) extends Map[String, User] {

  // Members declared in immutable.Map
  def +[V1 >: User](kv: (String, V1)): Map[String,V1] = ledger + kv
  // Members declared in MapLike
  def -(key: String): Map[String,User] = ledger - key
  def get(key: String): Option[User] = ledger.get(key)
  def iterator: Iterator[(String, User)] = ledger.iterator

  implicit def toLedger(m: Map[String, User]) = new Ledger(m)
  def rewardMiner(miner: String): Ledger = increase(miner, 1)
  def addUsers(newUsers: Set[String]): Ledger = (ledger /: newUsers) {
    (ledger, newUserName) => ledger + (newUserName -> User())
  }

  def applyTransactions(transactions: Seq[Transaction]): Ledger = {
    (ledger /: transactions) {
      (ledg, transaction) => {
        ledg.transfer(transaction.sender, transaction.recipient, transaction.amount)
      }
    }
  }

  def transfer(senderName: String, recipientName: String, amount: Int): Ledger = {
    increase(recipientName, amount).decrease(senderName, amount)
  }
  def decrease(userName: String, amount: Int): Ledger =
    changeBalance(userName, amount, _-_)
  def increase(userName: String, amount: Int): Ledger =
    changeBalance(userName, amount, _+_)

  private def changeBalance(userName: String, amount: Int, op: (Int, Int) => Int): Ledger = {
    require(amount > 0)
    val newBalance = op(ledger(userName).balance, amount)
    ledger + (userName -> User(newBalance))
  }
}
