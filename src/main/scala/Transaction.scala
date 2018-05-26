import Exceptions._

case class Transaction(val sender: String, val recipient: String, val amount: Int)
    extends SHAHashable {
  customRequire(sender != recipient, new IllegalTransactions("sender cannot also be recipient"))
  val hashDependencies = Seq[SHAHashable](sender, recipient, amount).map(_.hash)
}
