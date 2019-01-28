package raicoin

import java.net.InetSocketAddress

import akka.actor._
import akka.io.Tcp
import akka.testkit.TestProbe
import org.scalatest._
import raicoin.TestUtils._

import scala.concurrent.Await
import scala.concurrent.duration._

import sys.process._

object DockerComposeTag extends Tag("DockerComposeTag")

class ActorNetworking extends fixture.FunSuiteLike with fixture.ConfigMapFixture {

  test("remote actor automatically finds local actor and updates itself", DockerComposeTag) {
    configMap =>
      val system = ActorSystem("local")
      val actor = system.actorOf(
        Props(new BlockchainActor(testChains(0))(dockerBootstrapAware(vecnaConfig, configMap))))

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

  ignore("not mining actor forwards transaction to peers") { configMap =>
    val systemA = ActorSystem("A")
    val actorA  = systemA.actorOf(Props(new BlockchainActor(testChains(0))(bootstrapConfig)), "A")

    val system = ActorSystem("B")
    val actor  = system.actorOf(Props(new BlockchainActor(testChains(0))), "B")

    val p                      = TestProbe("p")(systemA)
    implicit val defaultSender = p.testActor

    Thread.sleep(500)
    actorA ! StartMining
    actor ! testTransactions(1)
    Thread.sleep(500)
    actorA ! RequestBlocksSince(1)
    try {
      assert(
        p.receiveWhile(1.seconds, 500.millis) { case msg => msg }
          .exists(msg => {
            tcpUnwrap[MinedBlock](msg.asInstanceOf[Tcp.Write]).transactions
              .contains(testTransactions(1).transaction)
          }))
    } finally {
      Seq(systemA, system).foreach(system => Await.result(system.terminate(), Duration.Inf))
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
