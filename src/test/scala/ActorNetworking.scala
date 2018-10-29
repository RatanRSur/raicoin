package raicoin

import akka.actor._
import akka.testkit.TestProbe
import org.scalatest._
import java.net.InetSocketAddress
import scala.concurrent.duration._
import scala.concurrent.Await
import BlockchainActor._
import Serializer._
import akka.io.Tcp
import TestUtils._

class ActorNetworking extends FunSuiteLike {

  test("remote actor automatically finds local actor and updates itself") {

    //bind to 6364
    val systemA = ActorSystem("A")
    val actorA  = systemA.actorOf(Props(new BlockchainActor(length4chain, tiamatPublicKey, None)), "A")

    //bind to 6363
    val systemB = ActorSystem("B")
    val actorB  = systemB.actorOf(Props(new BlockchainActor(rootOnly, vecnaPublicKey)), "B")

    val p                      = TestProbe("p")(systemB)
    implicit val defaultSender = p.testActor

    Thread.sleep(1000)
    retriesOnTimeout(1) {
      actorB ! Request(3)
      p.expectMsg(1.seconds, tcpWritten(length4chain(3)))
    }
    Seq(actorA, actorB).foreach(_ ! Disconnect)
    Seq(systemA, systemB).foreach(system => Await.result(system.terminate(), Duration.Inf))
  }

  test("B and C discover each other through A") {

      val systemA = ActorSystem("A")
      val actorA  = systemA.actorOf(Props(new BlockchainActor(length4chain, tiamatPublicKey, None)), "A")

      val systemB = ActorSystem("B")
      val actorB  = systemB.actorOf(Props(new BlockchainActor(length4chain, tiamatPublicKey)), "B")

      val systemC = ActorSystem("C")
      val actorC  = systemC.actorOf(Props(new BlockchainActor(length4chain, tiamatPublicKey)), "C")

      val p                      = TestProbe("p")(systemC)
      implicit val defaultSender = p.testActor

      Thread.sleep(500)
      retriesOnTimeout(1) {
        actorC ! GetPeers
        p.receiveN(2)
      }
      Seq(actorA, actorB, actorC).foreach(_ ! Disconnect)
      Seq(systemA, systemB, systemC).foreach(system => Await.result(system.terminate(), Duration.Inf))
  }

  test("changes propagate through a bigger system") {

    val systemA = ActorSystem("A")
    val actorA  = systemA.actorOf(Props(new BlockchainActor(rootOnly, tiamatPublicKey, None)), "A")

    val systemB = ActorSystem("B")
    val actorB  = systemB.actorOf(Props(new BlockchainActor(length4chain, tiamatPublicKey)), "B")

    Thread.sleep(500)

    val systemC = ActorSystem("C")
    val actorC  = systemC.actorOf(Props(new BlockchainActor(rootOnly, tiamatPublicKey)), "C")

    Thread.sleep(500)

    val p                      = TestProbe("p")(systemC)
    implicit val defaultSender = p.testActor

    actorC ! GetPeerInfo
    val cInfo = tcpUnwrap[PeerInfo](p.receiveN(1).head)

    val systemD = ActorSystem("D")
    val actorD  = systemD.actorOf(Props(new BlockchainActor(rootOnly, tiamatPublicKey, Some(cInfo))), "D")

    Thread.sleep(1000)
    retriesOnTimeout(1) {
      actorD ! Request(3)
      p.expectMsg(1.seconds, tcpWritten(length4chain(3)))
    }

    Seq(actorA, actorB, actorC, actorD).foreach(_ ! Disconnect)
    Seq(systemA, systemB, systemC, systemD).foreach(system => Await.result(system.terminate(), Duration.Inf))
  }

}
