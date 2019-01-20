package raicoin

import org.apache.commons.codec.binary.Hex
import org.scalatest._
import spray.json._
import spray.json.DefaultJsonProtocol._

import java.io.File
import org.apache.commons.io.FileUtils

import TestUtils._
import Serializer._
import RaicoinJsonProtocols._

class SerializationSpec extends FunSuite {
  def roundtrip[T](obj: T)(implicit format: JsonFormat[T]): T = {
    val tempFile = File.createTempFile(obj.getClass.toString, "")
    FileUtils.writeByteArrayToFile(tempFile, serialize[T](obj))
    deserialize[T](FileUtils.readFileToByteArray(tempFile))
  }

  def roundtripTest[T](obj: T)(implicit format: JsonFormat[T]) = {
    val roundtripped = roundtrip(obj)
    assert(obj == roundtripped)
  }

  test("serializable classes survive persistence") {
    val transaction = Transaction(tiamatPublicKey, vecnaPublicKey, 1)
    roundtripTest(transaction)
    val signedTransaction = transaction.sign(tiamatPrivateKey)
    roundtripTest(signedTransaction)
    val rootBlock = RootBlock()
    roundtripTest(rootBlock)
    val minedBlock = UnminedBlock(Hex.encodeHexString(rootBlock.hash),
                                  0,
                                  rootBlock.ledger,
                                  testTransactions,
                                  vecnaPublicKey,
                                  2).mine
    roundtripTest(minedBlock)
    val ledger = minedBlock.ledger
    roundtripTest(ledger)
  }
}
