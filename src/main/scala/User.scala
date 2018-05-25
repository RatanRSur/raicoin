case class User(val balance: Int = 0) extends SHAHashable {
  val hashDependencies = Seq(balance.hash)
}
