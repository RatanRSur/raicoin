package raicoin

import org.scalatest._
import akka.actor._
import akka.testkit.{ImplicitSender, TestActors, TestKit, TestProbe}
import akka.util.Timeout
import akka.io.Tcp
import akka.io.IO

import scala.concurrent.duration._
import scala.concurrent.Await

import TestUtils._

class BlockchainActorSpec extends FunSuiteLike with TestChains {

  val timeout = 10.millis

  test("adds received block to blockchain") {
    implicit val system        = ActorSystem()
    val blockchainActor        = system.actorOf(Props(new BlockchainActor(length2chain)))
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor
    blockchainActor ! length3chain.tip.block
    p.expectNoMessage(timeout)
    length3chain.zipWithIndex.foreach {
      case (block, i) => {
        blockchainActor ! Request(i)
        p.expectMsg(tcpWritten(block))
      }
    }
    system.terminate()
  }

  test("responds to request for one block") {
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(length2chain)))

    blockchainActor ! Request(1)
    p.expectMsg(tcpWritten(length2chain.tip.block))
    system.terminate()
  }

  test("can keep track of longest chain") {
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(length2chain)))
    val chainA = length2chain
      .mineBlock(Seq(Transaction("tiamat", "vecna", 1)), "tiamat")
    val chainB = length2chain
      .mineBlock(Seq(Transaction("tiamat", "vecna", 1)), "tiamat")
      .mineBlock(Seq(Transaction("vecna", "tiamat", 1)), "tiamat")

    blockchainActor ! chainA.tip.block
    blockchainActor ! Request(2)
    p.expectMsg(tcpWritten(chainA.tip.block))
    blockchainActor ! chainB(2)
    blockchainActor ! chainB(3)
    blockchainActor ! Request(2)
    p.expectMsg(tcpWritten(chainB(2)))
    system.terminate()
  }

  test("can handle blocks coming in in wrong order") {
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(rootOnly)))


    scala.util.Random.shuffle(0 to length4chain.height - 1).foreach {
      i => blockchainActor ! length4chain(i)
    }
    blockchainActor ! Request(3)
    p.expectMsg(tcpWritten(length4chain(3)))
    system.terminate()
  }

}
