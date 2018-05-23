case class Transaction(val sender: String, val recipient: String, val amount: Int) {
  val isValid = sender != recipient
}
