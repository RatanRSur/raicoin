package raicoin

import java.net.InetSocketAddress

import akka.actor._
import akka.io.{IO, Tcp}
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
    IO(Tcp) ! Tcp.Connect(dockerAddr("mining", configMap))
    p.expectMsgType[Tcp.Connected]
    val bootstrapRef = p.sender()

    bootstrapRef ! Tcp.Register(p.testActor)

    val testTx  = Transaction(vecnaPublicKey, tiamatPublicKey, 1)
    val testSTx = testTx.sign(vecnaPrivateKey)

    Thread.sleep(500)
    actor ! testSTx
    Thread.sleep(500)
    bootstrapRef ! Tcp.Write(toByteString(RequestBlocksSince(2)))
    try {
      val requestedBlocks = p
        .receiveWhile() { case Tcp.Received(data) => data }
        .toStream
        .flatMap(data => Try(deserialize[MinedBlock](data.toArray)).toOption)
      assert(
        requestedBlocks
          .exists(block => {
            block.transactions.contains(testTx)
          }))
    } finally {
      Await.result(system.terminate(), Duration.Inf)
    }
  }

  ignore("B and C discover each other through A") { configMap =>
    val systemA = ActorSystem("A")
    val actorA =
      systemA.actorOf(Props(new BlockchainActor(testChains(3))(bootstrapConfig)), "A")

    val system = ActorSystem("B")
    val actor  = system.actorOf(Props(new BlockchainActor(testChains(3))), "B")

    val systemC = ActorSystem("C")
    val actorC  = systemC.actorOf(Props(new BlockchainActor(testChains(3))), "C")

    val p                      = TestProbe("p")(systemC)
    implicit val defaultSender = p.testActor

    Thread.sleep(500)
    try {
      retriesOnTimeout(1) {
        actorC ! GetPeers
        p.receiveN(2)
      }
    } finally {
      //Seq(actorA, actor, actorC).foreach(_ ! Disconnect)
      Seq(systemA, system, systemC).foreach(system =>
        Await.result(system.terminate(), Duration.Inf))
    }
  }

  ignore("changes propagate through a bigger system") { configMap =>
    val systemA = ActorSystem("A")
    val actorA  = systemA.actorOf(Props(new BlockchainActor(testChains(0))(bootstrapConfig)), "A")

    val system = ActorSystem("B")
    val actor  = system.actorOf(Props(new BlockchainActor(testChains(3))), "B")

    Thread.sleep(500)

    val systemC = ActorSystem("C")
    val actorC  = systemC.actorOf(Props(new BlockchainActor(testChains(0))), "C")

    Thread.sleep(500)

    val p                      = TestProbe("p")(systemC)
    implicit val defaultSender = p.testActor

    actorC ! GetPeerInfo
    val cAddr = tcpUnwrap[InetSocketAddress](p.receiveN(1).head.asInstanceOf[Tcp.Write])

    val systemD = ActorSystem("D")
    val actorD =
      systemD.actorOf(
        Props(new BlockchainActor(testChains(0))(defaultConfig.copy(startingPeers = Seq(cAddr)))),
        "D")

    Thread.sleep(1000)
    try {
      retriesOnTimeout(1) {
        actorD ! Request(3)
        expectTcpMessage[MinedBlock](p, testChains(3)(3))
      }
    } finally {
      //Seq(actorA, actor, actorC, actorD).foreach(_ ! Disconnect)
      Seq(systemA, system, systemC, systemD).foreach(system =>
        Await.result(system.terminate(), Duration.Inf))
    }
  }

}
