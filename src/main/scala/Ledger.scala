import scala.collection.immutable.ListMap
import scala.collection.SortedSet
import scala.util.{Try, Success, Failure}
import Exceptions._

class Ledger(private val internalLedger: ListMap[String, Long] = ListMap.empty)
    extends Iterable[(String, Long)]
    with SHAHashable {

  //convenience
  def apply(x: String): Long              = internalLedger(x)
  def +(kv: (String, Long)): Ledger       = new Ledger(internalLedger + kv)
  def contains(userName: String): Boolean = internalLedger.contains(userName)
  //from Iterable
  val iterator = internalLedger.iterator

  def rewardMiner(miner: String): Ledger = increase(miner, 1)
  def addUsers(userNames: Seq[String]): Ledger = (this /: userNames) { (ledger, userName) =>
    customRequire(!ledger.contains(userName), new UserAlreadyExists(userName))
    ledger + (userName -> 0)
  }

  def applyTransactions(transactions: Seq[Transaction]): Try[Ledger] = {
    val ret = (this /: transactions) { (ledg, transaction) =>
      ledg.transfer(transaction.sender, transaction.recipient, transaction.amount)
    }
    Success(ret)
  }

  val hashDependencies = Seq[SHAHashable](internalLedger).map(_.hash)

  def transfer(senderName: String, recipientName: String, amount: Long): Ledger = {
    increase(recipientName, amount).decrease(senderName, amount)
  }
  private def decrease(userName: String, amount: Long): Ledger =
    changeBalance(userName, amount, _ - _)
  private def increase(userName: String, amount: Long): Ledger =
    changeBalance(userName, amount, _ + _)
  private def changeBalance(userName: String, amount: Long, op: (Long, Long) => Long): Ledger = {
    require(amount > 0)
    val newBalance = op(internalLedger(userName), amount)
    customRequire(newBalance >= 0, new IllegalTransactions(s"$userName would have $newBalance"))
    this + (userName -> newBalance)
  }
}
