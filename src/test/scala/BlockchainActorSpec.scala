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
    blockchainActor ! newLocalChain.last
    expectNoMessage(timeout)
    newLocalChain.foreach(block => {
      blockchainActor ! Request(block.index)
      expectMsg(block)
    })
  }

  test("adds blocks in correct order") {
    val chain =
      new Blockchain()
        .mineBlock(Seq(Transaction("vecna", "tiamat", 1)), "vecna", Seq("vecna", "tiamat"))
    val blockchainActor = system.actorOf(Props(new BlockchainActor(chain)))
    val newLocalChain = chain
      .mineBlock(Seq(Transaction("tiamat", "vecna", 1)), "tiamat")
      .mineBlock(Seq(Transaction("vecna", "tiamat", 1)), "tiamat")
    blockchainActor ! newLocalChain.last
    blockchainActor ! newLocalChain(newLocalChain.length - 2)
    expectNoMessage(timeout)
    newLocalChain.foreach(block => {
      blockchainActor ! Request(block.index)
      expectMsg(block)
    })
  }

  test("responds to request for one block") {
    val chain =
      new Blockchain()
        .mineBlock(Seq(Transaction("vecna", "tiamat", 1)), "vecna", Seq("vecna", "tiamat"))
    val blockchainActor = system.actorOf(Props(new BlockchainActor(chain)))

    blockchainActor ! Request(1)
    expectMsg(chain.last)
  }

}
