import org.scalatest._
import akka.actor._
import akka.testkit.{ImplicitSender, TestActors, TestKit}
import akka.util.Timeout

import scala.concurrent.duration._

class BlockchainActorSpec
    extends TestKit(ActorSystem("Blockchain"))
    with ImplicitSender
    with FunSuiteLike {

  val timeout     = 10.millis
  implicit val ec = system.dispatcher

  def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  test("adds received block to blockchain") {
    val chain =
      new Blockchain()
        .mineBlock(Seq(Transaction("vecna", "tiamat", 1)), "vecna", Seq("vecna", "tiamat"))
    val blockchainActor = system.actorOf(Props(new BlockchainActor(chain)))
    val newLocalChain   = chain.mineBlock(Seq(Transaction("tiamat", "vecna", 1)), "tiamat")
    blockchainActor ! newLocalChain.tip
    expectNoMessage(timeout)
    newLocalChain.foreach(block => {
      blockchainActor ! Request(block.index)
      expectMsg(block)
    })
  }

  test("throws away block that doesn't come in correct order") {
    val chain =
      new Blockchain()
        .mineBlock(Seq(Transaction("vecna", "tiamat", 1)), "vecna", Seq("vecna", "tiamat"))
    val blockchainActor = system.actorOf(Props(new BlockchainActor(chain)))
    val newLocalChain = chain
      .mineBlock(Seq(Transaction("tiamat", "vecna", 1)), "tiamat")
      .mineBlock(Seq(Transaction("vecna", "tiamat", 1)), "tiamat")
    blockchainActor ! newLocalChain.tip
    expectNoMessage(timeout)
    blockchainActor ! newLocalChain(newLocalChain.height - 2)
    (0 to 2).map(i => {
      blockchainActor ! Request(i)
      expectMsg(newLocalChain(i))
    })
    blockchainActor ! Request(3)
    expectNoMessage(timeout)
  }

  test("responds to request for one block") {
    val chain =
      new Blockchain()
        .mineBlock(Seq(Transaction("vecna", "tiamat", 1)), "vecna", Seq("vecna", "tiamat"))
    val blockchainActor = system.actorOf(Props(new BlockchainActor(chain)))

    blockchainActor ! Request(1)
    expectMsg(chain.tip)
  }

  test("can keep track of longest chain") {
    val chain =
      new Blockchain()
        .mineBlock(Seq(Transaction("vecna", "tiamat", 1)), "vecna", Seq("vecna", "tiamat"))
    val blockchainActor = system.actorOf(Props(new BlockchainActor(chain)))
    val chainA = chain
      .mineBlock(Seq(Transaction("tiamat", "vecna", 1)), "tiamat")
    val chainB = chain
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
