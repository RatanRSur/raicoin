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

  test("nodes update themselves with new blocks", DockerComposeTag) { configMap =>
    implicit val system        = ActorSystem("local")
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor
    val containerRef           = connectProbeToContainer(p, "island", configMap)

    containerRef ! Tcp.Write(toByteString(RequestBlocksSince(2)))
    try {
      val requestedBlocks = p
        .receiveWhile() { case Tcp.Received(data) => data }
        .toStream
        .flatMap(data => Try(fromByteString(data).asInstanceOf[MinedBlock]).toOption)
      assert(requestedBlocks.exists(_ == testChains(2)(2)))
    } finally {
      Await.result(system.terminate(), Duration.Inf)
    }
  }

  test("not mining actor forwards transaction to peers", DockerComposeTag) { configMap =>
    implicit val system = ActorSystem("local")

    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor
    val islandRef              = connectProbeToContainer(p, "island", configMap)
    val miningRef              = connectProbeToContainer(p, "mining", configMap)

    val testTx  = Transaction(vecnaPublicKey, tiamatPublicKey, 1)
    val testSTx = testTx.sign(vecnaPrivateKey)

    islandRef ! Tcp.Write(toByteString(testSTx))
    miningRef ! Tcp.Write(toByteString(RequestBlocksSince(2)))
    Thread.sleep(500)
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
    implicit val system        = ActorSystem("local")
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val containerRef = connectProbeToContainer(p, "island", configMap)

    Thread.sleep(500)
    try {
      retriesOnTimeout(1) {
        containerRef ! Tcp.Write(toByteString(GetPeers))
        val requestedPeers = p
          .receiveWhile() { case Tcp.Received(data) => data }
          .toStream
          .flatMap(data => Try(fromByteString(data).asInstanceOf[InetSocketAddress]).toOption)
        assert(requestedPeers.size > 1)
      }
    } finally {
      Await.result(system.terminate(), Duration.Inf)
    }
  }
}
