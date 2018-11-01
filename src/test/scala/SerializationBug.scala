package raicoin
import org.scalatest._
import org.apache.commons.lang3.SerializationUtils.serialize

import TestUtils._

class SerializationBug extends FunSuiteLike {
  test("verification remains true"){
    val tx1 = Transaction(vecnaPublicKey, tiamatPublicKey, 1)
    val signedTx1 = tx1.sign(vecnaPrivateKey)
    assert(signedTx1.verify)
    signedTx1.transaction.hash
    assert(signedTx1.verify)
  }

  test("RootBlook toString does not change serialization"){
    val rb = new RootBlock()
    val bytesBeforeToString = serialize(rb)
    rb.toString
    assert(serialize(rb) === bytesBeforeToString)
  }
}
