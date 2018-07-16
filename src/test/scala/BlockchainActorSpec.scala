package ratan.blockchain

import org.scalatest._
import akka.actor._
import akka.testkit.{ImplicitSender, TestActors, TestKit, TestProbe}
import akka.util.Timeout
import akka.io.Tcp
import akka.io.IO

import scala.concurrent.duration._
import scala.concurrent.Await

class BlockchainActorSpec extends FunSuiteLike with TestChains {

  val timeout = 10.millis

  ignore("adds received block to blockchain") {
    implicit val system        = ActorSystem()
    val blockchainActor        = system.actorOf(Props(new BlockchainActor(length2chain)))
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor
    blockchainActor ! length3chain.tip
    p.expectNoMessage(timeout)
    length3chain.zipWithIndex.foreach {
      case (block, i) => {
        blockchainActor ! Request(i)
        p.expectMsg(block)
      }
    }
    system.terminate()
  }

  ignore("throws away block that doesn't come in correct order") {
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(length2chain)))
    blockchainActor ! length4chain.tip
    p.expectNoMessage(timeout)
    blockchainActor ! length4chain(length4chain.height - 2)
    (0 to 2).map(i => {
      blockchainActor ! Request(i)
      p.expectMsg(length4chain(i))
    })
    blockchainActor ! Request(3)
    p.expectNoMessage(timeout)
    system.terminate()
  }

  ignore("responds to request for one block") {
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(length2chain)))

    blockchainActor ! Request(1)
    p.expectMsg(length2chain.tip)
    system.terminate()
  }

  ignore("can keep track of longest chain") {
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(length2chain)))
    val chainA = length2chain
      .mineBlock(Seq(Transaction("tiamat", "vecna", 1)), "tiamat")
    val chainB = length2chain
      .mineBlock(Seq(Transaction("tiamat", "vecna", 1)), "tiamat")
      .mineBlock(Seq(Transaction("vecna", "tiamat", 1)), "tiamat")

    blockchainActor ! chainA.tip
    blockchainActor ! Request(2)
    p.expectMsg(chainA.tip)
    blockchainActor ! chainB(2)
    blockchainActor ! chainB(3)
    blockchainActor ! Request(2)
    p.expectMsg(chainB(2))
    system.terminate()
  }

}
