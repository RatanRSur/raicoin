class Ledger(
  private val ledger: Map[String, User],
  val negativeAccounts: Set[String] = Set.empty) extends Map[String, User] {

  val isValid = negativeAccounts.isEmpty

  implicit def toLedger(m: Map[String, User]) = new Ledger(m, negativeAccounts)
  // Members declared in immutable.Map
  def +[V1 >: User](kv: (String, V1)): Ledger = {
    val (userName, user) = kv
    user match {
      case u: User => new Ledger(ledger + (userName -> u), negativeAccounts)
      case _ => throw new IllegalArgumentException()
    }
  }
  // Members declared in MapLike
  def -(key: String): Ledger = new Ledger(ledger - key, negativeAccounts - key)
  def get(key: String): Option[User] = ledger.get(key)
  def iterator: Iterator[(String, User)] = ledger.iterator

  def rewardMiner(miner: String): Ledger = increase(miner, 1)
  def addUsers(userNames: Set[String]): Ledger = (ledger /: userNames) {
    (ledger, userName) => ledger + (userName -> User())
  }

  def applyTransactions(transactions: Seq[Transaction]): Ledger = {
    (this /: transactions) {
      (ledg, transaction) => {
        ledg.transfer(transaction.sender, transaction.recipient, transaction.amount)
      }
    }
  }

  private def transfer(senderName: String, recipientName: String, amount: Int): Ledger = {
    increase(recipientName, amount).decrease(senderName, amount)
  }


  private def decrease(userName: String, amount: Int): Ledger = changeBalance(userName, amount, _-_)
  private def increase(userName: String, amount: Int): Ledger = changeBalance(userName, amount, _+_)
  private def changeBalance(userName: String, amount: Int, op: (Int, Int) => Int): Ledger = {
    require(amount > 0)
    val newBalance = op(ledger(userName).balance, amount)
    val newLedger = if (newBalance < 0) inTheRed(userName)
                    else                inTheBlack(userName)
    newLedger + (userName -> User(newBalance))
  }

  private def inTheBlack(userName: String): Ledger = new Ledger(ledger, negativeAccounts - userName)
  private def inTheRed(userName: String): Ledger = new Ledger(ledger, negativeAccounts + userName)
}
