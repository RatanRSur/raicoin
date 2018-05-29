import org.scalatest._
import akka.actor._
import akka.pattern.ask
import akka.testkit.{ImplicitSender, TestActors, TestKit}
import akka.util.Timeout

import scala.concurrent.duration._

class BlockchainActorSpec
    extends TestKit(ActorSystem("Blockchain"))
    with ImplicitSender
    with FunSuiteLike {

  implicit val timeout = Timeout(300.millis)
  implicit val ec      = system.dispatcher

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
    expectNoMessage(300.millis)
  }

  test("broadcasted chain has the same hashes") {
    val chain =
      new Blockchain()
        .mineBlock(Seq(Transaction("vecna", "tiamat", 1)), "vecna", Seq("vecna", "tiamat"))
    val blockchainActor = system.actorOf(Props(new BlockchainActor(chain)))
    blockchainActor ! Broadcast
    chain.foreach(block => expectMsg(block))
    (blockchainActor ? Blockchain)
      .mapTo[Blockchain]
      .foreach(broadcastedChain =>
        chain.zip(broadcastedChain).foreach {
          case (b1, b2) => assert(b1.hash === b2.hash)
      })
  }

  test("Blockchain actor confirms receipt of block") {
    val chain =
      new Blockchain()
        .mineBlock(Seq(Transaction("vecna", "tiamat", 1)), "vecna", Seq("vecna", "tiamat"))
    val blockchainActor = system.actorOf(Props(new BlockchainActor(chain)))
    val newLocalChain   = chain.mineBlock(Seq(Transaction("tiamat", "vecna", 1)), "tiamat")
    blockchainActor ! newLocalChain.last
    expectMsg(Received(2, newLocalChain.last.hashHex))
  }

}
