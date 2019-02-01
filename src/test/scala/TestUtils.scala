package raicoin

import akka.actor._
import akka.io.{IO, Tcp}
import akka.testkit.TestProbe
import raicoin.Serializer._
import scorex.crypto.signatures._
import java.net.InetSocketAddress
import java.io.File
import org.scalatest.ConfigMap
import java.net.InetAddress

object TestUtils {

  val (tiamatPrivateKey, tiamatPublicKey) = Curve25519.createKeyPair(Array(0))
  val (vecnaPrivateKey, vecnaPublicKey)   = Curve25519.createKeyPair(Array(1))

  val bootstrapAddress = new InetSocketAddress("bootstrap", Config.defaultPort)
  implicit val defaultConfig = Config(privateKey = tiamatPrivateKey,
                                      publicKey = tiamatPublicKey,
                                      startingPeers = Seq(bootstrapAddress))
  val vecnaConfig     = defaultConfig.copy(privateKey = vecnaPrivateKey, publicKey = vecnaPublicKey)
  val bootstrapConfig = defaultConfig.copy(startingPeers = Nil)

  val testTransactions =
    Seq(
      Transaction(vecnaPublicKey, tiamatPublicKey, 1, 1548171779510L),
      Transaction(tiamatPublicKey, vecnaPublicKey, 1, 1548171779554L),
      Transaction(vecnaPublicKey, tiamatPublicKey, 1, 1548171779598L)
    ).map { tx =>
      val keyToSignWith =
        if (tx.sender.deep == tiamatPublicKey.deep) tiamatPrivateKey else vecnaPrivateKey
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

  def dockerAddr(containerName: String, configMap: ConfigMap): InetSocketAddress = {
    val splitStringRepr = configMap(s"$containerName:6363").asInstanceOf[String].split(':')
    new InetSocketAddress(InetAddress.getByName(splitStringRepr(0)), splitStringRepr(1).toInt)
  }

  def dockerAddrAware(containerNames: Seq[String],
                      config: Config,
                      scalatestConfigMap: ConfigMap): Config = {
    config.copy(
      startingPeers =
        config.startingPeers ++ containerNames.map(name => dockerAddr(name, scalatestConfigMap)))
  }

  def dockerAddrAware(containerName: String,
                      config: Config,
                      scalatestConfigMap: ConfigMap): Config = {
    dockerAddrAware(Seq(containerName), config, scalatestConfigMap)
  }

  def connectProbeToContainer(p: TestProbe, containerName: String, configMap: ConfigMap)(
      implicit system: ActorSystem): ActorRef = {
    implicit val defaultSender = p.testActor
    IO(Tcp) ! Tcp.Connect(dockerAddr(containerName, configMap))
    p.expectMsgType[Tcp.Connected]
    val containerRef = p.sender()
    containerRef ! Tcp.Register(p.testActor)
    containerRef
  }
}
