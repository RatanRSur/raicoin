package raicoin

import java.net.InetSocketAddress

import akka.actor._
import akka.io.Tcp
import akka.testkit.TestProbe
import org.scalatest._
import raicoin.TestUtils._

import scala.concurrent.Await
import scala.concurrent.duration._

class ActorNetworking extends FunSuiteLike {

  test("remote actor automatically finds local actor and updates itself") {

    val systemA = ActorSystem("A")
    val actorA =
      systemA.actorOf(Props(new BlockchainActor(length4chain)(bootstrapConfig)), "A")

    val systemB = ActorSystem("B")
    val actorB  = systemB.actorOf(Props(new BlockchainActor(rootOnly)(vecnaConfig)), "B")

    val p                      = TestProbe("p")(systemB)
    implicit val defaultSender = p.testActor

    Thread.sleep(1000)
    try {
      retriesOnTimeout(1) {
        actorB ! Request(3)
        expectTcpMessage(p, length4chain(3))
      }
    } finally {
      Seq(actorA, actorB).foreach(_ ! Disconnect)
      Seq(systemA, systemB).foreach(system => Await.result(system.terminate(), Duration.Inf))
    }
  }

  test("not mining actor forwards transaction to peers") {

    val systemA = ActorSystem("A")
    val actorA  = systemA.actorOf(Props(new BlockchainActor(rootOnly)(bootstrapConfig)), "A")

    val systemB = ActorSystem("B")
    val actorB  = systemB.actorOf(Props(new BlockchainActor(rootOnly)), "B")

    val p                      = TestProbe("p")(systemA)
    implicit val defaultSender = p.testActor

    Thread.sleep(500)
    actorA ! StartMining
    actorB ! testTransactions(1)
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
      Seq(actorA, actorB).foreach(_ ! Disconnect)
      Seq(systemA, systemB).foreach(system => Await.result(system.terminate(), Duration.Inf))
    }
  }

  test("B and C discover each other through A") {

    val systemA = ActorSystem("A")
    val actorA =
      systemA.actorOf(Props(new BlockchainActor(length4chain)(bootstrapConfig)), "A")

    val systemB = ActorSystem("B")
    val actorB  = systemB.actorOf(Props(new BlockchainActor(length4chain)), "B")

    val systemC = ActorSystem("C")
    val actorC  = systemC.actorOf(Props(new BlockchainActor(length4chain)), "C")

    val p                      = TestProbe("p")(systemC)
    implicit val defaultSender = p.testActor

    Thread.sleep(500)
    try {
      retriesOnTimeout(1) {
        actorC ! GetPeers
        p.receiveN(2)
      }
    } finally {
      Seq(actorA, actorB, actorC).foreach(_ ! Disconnect)
      Seq(systemA, systemB, systemC).foreach(system =>
        Await.result(system.terminate(), Duration.Inf))
    }
  }

  test("changes propagate through a bigger system") {

    val systemA = ActorSystem("A")
    val actorA  = systemA.actorOf(Props(new BlockchainActor(rootOnly)(bootstrapConfig)), "A")

    val systemB = ActorSystem("B")
    val actorB  = systemB.actorOf(Props(new BlockchainActor(length4chain)), "B")

    Thread.sleep(500)

    val systemC = ActorSystem("C")
    val actorC  = systemC.actorOf(Props(new BlockchainActor(rootOnly)), "C")

    Thread.sleep(500)

    val p                      = TestProbe("p")(systemC)
    implicit val defaultSender = p.testActor

    actorC ! GetPeerInfo
    val cAddr = tcpUnwrap[InetSocketAddress](p.receiveN(1).head.asInstanceOf[Tcp.Write])

    val systemD = ActorSystem("D")
    val actorD =
      systemD.actorOf(Props(new BlockchainActor(rootOnly)(defaultConfig.copy(startingPeers = Seq(cAddr)))), "D")

    Thread.sleep(1000)
    try {
      retriesOnTimeout(1) {
        actorD ! Request(3)
        expectTcpMessage[MinedBlock](p, length4chain(3))
      }
    } finally {
      Seq(actorA, actorB, actorC, actorD).foreach(_ ! Disconnect)
      Seq(systemA, systemB, systemC, systemD).foreach(system =>
        Await.result(system.terminate(), Duration.Inf))
    }
  }

}
