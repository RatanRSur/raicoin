package raicoin

import java.net.InetSocketAddress

import akka.actor._
import akka.io.Tcp
import akka.testkit.TestProbe
import org.scalatest._
import TestUtils._
import Serializer._
import RaicoinJsonProtocols._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

import sys.process._

object DockerComposeTag extends Tag("DockerComposeTag")

class ActorNetworking extends fixture.FunSuiteLike with fixture.ConfigMapFixture {

  test("remote actor automatically finds local actor and updates itself", DockerComposeTag) {
    configMap =>
      val system = ActorSystem("local")
      val actor = system.actorOf(
        Props(
          new BlockchainActor(testChains(0))(dockerAddrAware("bootstrap", vecnaConfig, configMap))))

      val p                      = TestProbe("p")(system)
      implicit val defaultSender = p.testActor

      Thread.sleep(1000)
      try {
        retriesOnTimeout(1) {
          actor ! Request(2)
          expectTcpMessage(p, testChains(2)(2))
        }
      } finally {
        Await.result(system.terminate(), Duration.Inf)
      }
  }

  test("not mining actor forwards transaction to peers", DockerComposeTag) { configMap =>
    implicit val system = ActorSystem("local")
    val actor = system.actorOf(
      Props(new BlockchainActor(testChains(3))(dockerAddrAware("mining", vecnaConfig, configMap))))

    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor
    val containerRef           = connectProbeToContainer(p, "mining", configMap)

    val testTx  = Transaction(vecnaPublicKey, tiamatPublicKey, 1)
    val testSTx = testTx.sign(vecnaPrivateKey)

    Thread.sleep(500)
    actor ! testSTx
    Thread.sleep(500)
    containerRef ! Tcp.Write(toByteString(RequestBlocksSince(2)))
    try {
      val requestedBlocks = p
        .receiveWhile() { case Tcp.Received(data) => data }
        .toStream
        .flatMap(data => Try(fromByteString(data).asInstanceOf[MinedBlock]).toOption)
      assert(
        requestedBlocks
          .exists(block => {
            block.transactions.contains(testTx)
          }))
    } finally {
      Await.result(system.terminate(), Duration.Inf)
    }
  }

  test("peer discovery", DockerComposeTag) { configMap =>
    implicit val system = ActorSystem("local")
    val actor = system.actorOf(
      Props(new BlockchainActor(testChains(3))(
        dockerAddrAware(Seq("bootstrap", "mining"), vecnaConfig, configMap))))

    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val containerRef = connectProbeToContainer(p, "bootstrap", configMap)

    Thread.sleep(500)
    try {
      retriesOnTimeout(1) {
        containerRef ! Tcp.Write(toByteString(GetPeers))
        val requestedPeers = p
          .receiveWhile() { case Tcp.Received(data) => data }
          .toStream
          .flatMap(data => Try(fromByteString(data).asInstanceOf[InetSocketAddress]).toOption)
        assert(requestedPeers.length === 3, requestedPeers.toArray.deep)
      }
    } finally {
      Await.result(system.terminate(), Duration.Inf)
    }
  }
}
