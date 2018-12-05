package raicoin

import scala.concurrent.duration._

import akka.io.Tcp
import Serializer._

import scorex.crypto._
import scorex.crypto.signatures._

import akka.testkit._


object TestUtils {

  val (tiamatPrivateKey, tiamatPublicKey): (PrivateKey, PublicKey) = Curve25519.createKeyPair
  val (vecnaPrivateKey, vecnaPublicKey): (PrivateKey, PublicKey) = Curve25519.createKeyPair

  val testTransactions =
    Seq(Transaction(vecnaPublicKey, tiamatPublicKey, 1),
        Transaction(tiamatPublicKey, vecnaPublicKey, 1),
        Transaction(vecnaPublicKey, tiamatPublicKey, 1))
    .map { tx =>
      val keyToSignWith =
        if (tx.sender == tiamatPublicKey) tiamatPrivateKey else vecnaPrivateKey
      tx.sign(keyToSignWith)
    }

  val rootOnly = new Blockchain(difficulty = 1)
  val length2chain =
    rootOnly.mineBlock(Seq(testTransactions(0)), vecnaPublicKey)
  val length3chain = length2chain.mineBlock(Seq(testTransactions(1)), tiamatPublicKey)
  val length4chain = length3chain.mineBlock(Seq(testTransactions(2)), tiamatPublicKey)

  def tcpWritten(obj: Serializable) = {
    Tcp.Write(toByteString(obj))
  }

  def retriesOnTimeout[T](n: Int)(block: =>T): T = {
    require(n >= 0)
    try {
      block
    } catch {
      case ae: AssertionError => if (ae.getMessage.contains("timeout") && n > 0) {
        retriesOnTimeout(n-1)(block)
      } else throw ae
    }
  }

  def receiveNonTcpMessage(probe: TestProbe, max: Duration) = {
    val msg = probe.receiveOne(max)
    if (msg.isInstanceOf[Tcp.Event] || msg.isInstanceOf[Tcp.Write]) probe.receiveOne(max)
    else msg
  }


  def tcpUnwrap[T](supposedlyWrite: Any): T = {
    fromByteString(supposedlyWrite.asInstanceOf[Tcp.Write].data).asInstanceOf[T]
  }
}
