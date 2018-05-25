import scala.collection.immutable.ListMap
import scala.collection.SortedSet

class Ledger(
  private val internalLedger: ListMap[String, User] = ListMap.empty,
  val negativeAccounts: SortedSet[String] = SortedSet.empty) extends Iterable[(String, User)] with SHAHashable {

  //convenience
  def apply(x: String): User = internalLedger(x)
  def +(kv: (String, User)): Ledger = {
    val (userName, balance) = (kv._1, kv._2.balance)
    val newNegativeAccounts = if (balance >= 0) negativeAccounts - userName
                              else negativeAccounts + userName
    new Ledger(internalLedger + kv, newNegativeAccounts)
  }
  def contains(userName: String): Boolean = internalLedger.contains(userName)
  //from Iterable
  val iterator = internalLedger.iterator

  val isValid = negativeAccounts.isEmpty

  def rewardMiner(miner: String): Ledger = increase(miner, 1)
  def addUsers(userNames: Seq[String]): Ledger = (this /: userNames) {
    (ledger, userName) => {
      require(!ledger.contains(userName))
      ledger + (userName -> User())
    }
  }

  def applyTransactions(transactions: Seq[Transaction]): Ledger = {
    (this /: transactions) {
      (ledg, transaction) => {
        ledg.transfer(transaction.sender, transaction.recipient, transaction.amount)
      }
    }
  }

  val hashDependencies = Seq[SHAHashable](internalLedger, negativeAccounts).map(_.hash)


  private def transfer(senderName: String, recipientName: String, amount: Int): Ledger = {
    increase(recipientName, amount).decrease(senderName, amount)
  }
  private def decrease(userName: String, amount: Int): Ledger = changeBalance(userName, amount, _-_)
  private def increase(userName: String, amount: Int): Ledger = changeBalance(userName, amount, _+_)
  private def changeBalance(userName: String, amount: Int, op: (Int, Int) => Int): Ledger = {
    require(amount > 0)
    val newBalance = op(internalLedger(userName).balance, amount)
    this + (userName -> User(newBalance))
  }
}
