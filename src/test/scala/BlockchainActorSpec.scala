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

  test("broadcasts only unbroadcasted blocks") {
    val chain =
      new Blockchain()
        .mineBlock(Seq(Transaction("vecna", "tiamat", 1)), "vecna", Seq("vecna", "tiamat"))
    val blockchainActor = system.actorOf(Props(new BlockchainActor(chain)))

    blockchainActor ! Broadcast
    chain.foreach(block => expectMsg(block))
    blockchainActor ! Broadcast
    expectNoMessage(timeout)
  }

  test("broadcasted blocks are same as local blocks") {
    val chain =
      new Blockchain()
        .mineBlock(Seq(Transaction("vecna", "tiamat", 1)), "vecna", Seq("vecna", "tiamat"))
    val blockchainActor = system.actorOf(Props(new BlockchainActor(chain)))
    blockchainActor ! Broadcast
    chain.map(block => expectMsg(block))
  }

  test("adds received block to blockchain") {
    val chain =
      new Blockchain()
        .mineBlock(Seq(Transaction("vecna", "tiamat", 1)), "vecna", Seq("vecna", "tiamat"))
    val blockchainActor = system.actorOf(Props(new BlockchainActor(chain)))
    val newLocalChain   = chain.mineBlock(Seq(Transaction("tiamat", "vecna", 1)), "tiamat")
    blockchainActor ! newLocalChain.last
    expectNoMessage(timeout)
    blockchainActor ! Broadcast
    newLocalChain.map(block => expectMsg(block))
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
    blockchainActor ! Broadcast
    newLocalChain.map(block => expectMsg(block))
  }

}
