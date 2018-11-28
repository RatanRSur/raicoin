package raicoin

import scala.collection.immutable.ListMap
import scala.collection.SortedSet
import scala.util.{Try, Success, Failure}
import scorex.crypto.signatures._
import Exceptions._

class Ledger(private val internalLedger: ListMap[PublicKey, Long] = ListMap.empty)
    extends Iterable[(PublicKey, Long)]
    with SHAHashable
    with Serializable {

  //convenience
  def apply(x: PublicKey): Long              = internalLedger.getOrElse(x, 0)
  def +(kv: (PublicKey, Long)): Ledger       = new Ledger(internalLedger + kv)
  def contains(pk: PublicKey): Boolean = internalLedger.contains(pk)
  //from Iterable
  def iterator = internalLedger.iterator

  def rewardMiner(miner: PublicKey): Ledger = increase(miner, 1)

  def applyTransactions(transactions: Seq[Transaction]): Try[Ledger] = {
    val ret = (this /: transactions) { (ledg, transaction) =>
      ledg.transfer(transaction.sender, transaction.recipient, transaction.amount)
    }
    Success(ret)
  }

  val hashDependencies = Seq[SHAHashable](internalLedger).map(_.hash)

  def transfer(senderName: PublicKey, recipientName: PublicKey, amount: Long): Ledger = {
    increase(recipientName, amount).decrease(senderName, amount)
  }
  private def decrease(pk: PublicKey, amount: Long): Ledger =
    changeBalance(pk, amount, _ - _)
  private def increase(pk: PublicKey, amount: Long): Ledger =
    changeBalance(pk, amount, _ + _)
  private def changeBalance(pk: PublicKey, amount: Long, op: (Long, Long) => Long): Ledger = {
    require(amount > 0)
    val newBalance = op(this(pk), amount)
    customRequire(newBalance >= 0, new IllegalTransactions(s"$pk would have $newBalance"))
    this + (pk -> newBalance)
  }
}
