package raicoin

import TestUtils._

trait TestChains {

  val rootOnly = new Blockchain()
  val length2chain =
    rootOnly.mineBlock(Seq(Transaction(vecnaPublicKey, tiamatPublicKey, 1)), vecnaPublicKey, Seq(vecnaPublicKey, tiamatPublicKey))
  val length3chain = length2chain.mineBlock(Seq(Transaction(tiamatPublicKey, vecnaPublicKey, 1)), tiamatPublicKey)
  val length4chain = length3chain.mineBlock(Seq(Transaction(vecnaPublicKey, tiamatPublicKey, 1)), tiamatPublicKey)
}
