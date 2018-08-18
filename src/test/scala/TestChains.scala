package raicoin

trait TestChains {
  val rootOnly = new Blockchain()
  val length2chain =
    rootOnly.mineBlock(Seq(Transaction("vecna", "tiamat", 1)), "vecna", Seq("vecna", "tiamat"))
  val length3chain = length2chain.mineBlock(Seq(Transaction("tiamat", "vecna", 1)), "tiamat")
  val length4chain = length3chain.mineBlock(Seq(Transaction("vecna", "tiamat", 1)), "tiamat")
}
