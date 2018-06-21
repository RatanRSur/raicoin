import org.scalatest._
import akka.actor._
import akka.testkit.{ImplicitSender, TestActors, TestKit}
import akka.util.Timeout

import scala.concurrent.duration._

class BlockchainActorSpec
    extends TestKit(ActorSystem("Blockchain"))
    with ImplicitSender
    with FunSuiteLike
    with TestChains {

  val timeout     = 10.millis
  implicit val ec = system.dispatcher

  def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  test("adds received block to blockchain") {
    val blockchainActor = system.actorOf(Props(new BlockchainActor(length2chain)))
    blockchainActor ! length3chain.tip
    expectNoMessage(timeout)
    length3chain.foreach(block => {
      blockchainActor ! Request(block.index)
      expectMsg(block)
    })
  }

  test("throws away block that doesn't come in correct order") {
    val blockchainActor = system.actorOf(Props(new BlockchainActor(length2chain)))
    blockchainActor ! length4chain.tip
    expectNoMessage(timeout)
    blockchainActor ! length4chain(length4chain.height - 2)
    (0 to 2).map(i => {
      blockchainActor ! Request(i)
      expectMsg(length4chain(i))
    })
    blockchainActor ! Request(3)
    expectNoMessage(timeout)
  }

  test("responds to request for one block") {
    val blockchainActor = system.actorOf(Props(new BlockchainActor(length2chain)))

    blockchainActor ! Request(1)
    expectMsg(length2chain.tip)
  }

  test("can keep track of longest chain") {
    val blockchainActor = system.actorOf(Props(new BlockchainActor(length2chain)))
    val chainA = length2chain
      .mineBlock(Seq(Transaction("tiamat", "vecna", 1)), "tiamat")
    val chainB = length2chain
      .mineBlock(Seq(Transaction("tiamat", "vecna", 1)), "tiamat")
      .mineBlock(Seq(Transaction("vecna", "tiamat", 1)), "tiamat")

    blockchainActor ! chainA.tip
    blockchainActor ! Request(2)
    expectMsg(chainA.tip)
    blockchainActor ! chainB(2)
    blockchainActor ! chainB(3)
    blockchainActor ! Request(2)
    expectMsg(chainB(2))
  }

}
