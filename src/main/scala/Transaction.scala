case class Transaction(val sender: String, val recipient: String, val amount: Int) extends SHAHashable {
  val isValid = sender != recipient
  val hashDependencies = Seq[SHAHashable](sender, recipient, amount).map(_.hash)
}
