package raicoin

import akka.io.Tcp
import akka.testkit.TestProbe
import raicoin.Serializer._
import scorex.crypto.signatures._
import java.net.InetSocketAddress
import java.io.File

object TestUtils {

  val (tiamatPrivateKey, tiamatPublicKey): (PrivateKey, PublicKey) = Curve25519.createKeyPair
  val (vecnaPrivateKey, vecnaPublicKey): (PrivateKey, PublicKey)   = Curve25519.createKeyPair

  val bootstrapAddress = new InetSocketAddress("bootstrap", Config.defaultPort)
  implicit val defaultConfig = Config(privateKey = tiamatPrivateKey,
                                      publicKey = tiamatPublicKey,
                                      startingPeers = Seq(bootstrapAddress))
  val vecnaConfig     = defaultConfig.copy(privateKey = vecnaPrivateKey, publicKey = vecnaPublicKey)
  val bootstrapConfig = defaultConfig.copy(startingPeers = Nil)

  val testTransactions =
    Seq(Transaction(vecnaPublicKey, tiamatPublicKey, 1),
        Transaction(tiamatPublicKey, vecnaPublicKey, 1),
        Transaction(vecnaPublicKey, tiamatPublicKey, 1))
      .map { tx =>
        val keyToSignWith =
          if (tx.sender == tiamatPublicKey) tiamatPrivateKey else vecnaPrivateKey
        tx.sign(keyToSignWith)
      }

  lazy val testChains =
    (1 to 4).map(i => Blockchain.fromFile(new File(s"src/test/scala/resources/length$i.chain")))

  def retriesOnTimeout[T](n: Int)(block: => T): T = {
    require(n >= 0)
    try {
      block
    } catch {
      case ae: AssertionError =>
        if (ae.getMessage.contains("timeout") && n > 0) {
          retriesOnTimeout(n - 1)(block)
        } else throw ae
    }
  }

  def tcpUnwrap[T](supposedlyWrite: Tcp.Write): T = {
    fromByteString(supposedlyWrite.data).asInstanceOf[T]
  }

  def receiveOneTcpMessage[T](p: TestProbe) = {
    tcpUnwrap[T](p.receiveN(1).head.asInstanceOf[Tcp.Write])
  }

  def expectTcpMessage[T](p: TestProbe, msg: Any) = {
    val tcpMessage = receiveOneTcpMessage[T](p)
    assert(msg == tcpMessage, s"Expected: ${msg.toString}\nGot: ${tcpMessage.toString}")
  }
}
