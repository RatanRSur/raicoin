package raicoin
import org.scalatest._

import TestUtils._

class SerializationBug extends FunSuiteLike {
  test("verification remains true"){
    val tx1 = Transaction(vecnaPublicKey, tiamatPublicKey, 1)
    val signedTx1 = tx1.sign(vecnaPrivateKey)
    assert(signedTx1.verify)
    signedTx1.transaction.hash
    assert(signedTx1.verify)
  }
}
