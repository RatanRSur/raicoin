package raicoin

import org.scalatest._
import akka.actor._
import akka.testkit.{ImplicitSender, TestActors, TestKit, TestProbe}
import akka.util.Timeout
import akka.io.Tcp
import akka.io.IO

import scorex.crypto.signatures._

import scala.concurrent.duration._
import scala.concurrent.Await

import java.nio.file.{Files, Paths}

import TestUtils._

class BlockchainActorSpec extends FunSuiteLike {

  val timeout = 10.millis

  test("adds received block to blockchain") {
    implicit val system        = ActorSystem()
    val blockchainActor        = system.actorOf(Props(new BlockchainActor(length2chain, tiamatPublicKey)))
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

    val blockchainActor = system.actorOf(Props(new BlockchainActor(length2chain, tiamatPublicKey)))

    blockchainActor ! Request(1)
    p.expectMsg(tcpWritten(length2chain.tip.block))
    system.terminate()
  }

  test("can keep track of longest chain") {
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(length2chain, tiamatPublicKey)))
    val chainA = length2chain
      .mineBlock(Seq(testTransactions(1)), tiamatPublicKey)
    val chainB = length2chain
      .mineBlock(Seq(testTransactions(1)), tiamatPublicKey)
      .mineBlock(Seq(testTransactions(2)), tiamatPublicKey)

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

    val blockchainActor = system.actorOf(Props(new BlockchainActor(rootOnly, tiamatPublicKey)))


    scala.util.Random.shuffle(0 to length4chain.height - 1).foreach {
      i => blockchainActor ! length4chain(i)
    }
    blockchainActor ! Request(3)
    p.expectMsg(tcpWritten(length4chain(3)))
    system.terminate()
  }

  test("mines block iff valid transaction received and is mining"){
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(length2chain, tiamatPublicKey)))

    val transaction = Transaction(tiamatPublicKey, vecnaPublicKey, 1)
    val signedTransaction = transaction.sign(tiamatPrivateKey)
    blockchainActor ! StartMining
    blockchainActor ! signedTransaction
    blockchainActor ! Request(2)
    assert(tcpUnwrap[MinedBlock](p.receiveOne(1.seconds)).transactions.contains(transaction))
    blockchainActor ! StopMining
    blockchainActor ! signedTransaction
    blockchainActor ! Request(3)
    assert(!tcpUnwrap[MinedBlock](p.receiveOne(1.seconds)).transactions.contains(transaction))
    system.terminate()
  }

  test("does not mine block when invalid transaction received"){
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(length2chain, tiamatPublicKey)))

    val transaction = Transaction(tiamatPublicKey, vecnaPublicKey, 1)
    val invalidTransaction = transaction.sign(vecnaPrivateKey)
    blockchainActor ! StartMining
    blockchainActor ! invalidTransaction
    blockchainActor ! Request(2)
    p.expectNoMessage()
    system.terminate()
  }

  test("rejects block with invalid transactions"){
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(length2chain, tiamatPublicKey)))

    val transaction = Transaction(tiamatPublicKey, vecnaPublicKey, 1)
    val invalidTransaction = transaction.sign(vecnaPrivateKey)
    val invalidBlock =
      length2chain.mineBlock(Seq(invalidTransaction), vecnaPublicKey).tip.block
    val validTransaction = transaction.sign(tiamatPrivateKey)
    val validBlock =
      length2chain.mineBlock(Seq(validTransaction), vecnaPublicKey).tip.block

    blockchainActor ! invalidBlock
    blockchainActor ! Request(2)
    p.expectNoMessage(1.seconds)
    blockchainActor ! validBlock
    blockchainActor ! Request(2)
    p.receiveOne(500.millis)
    system.terminate()
  }

  test("mining blockchain should have some value in account by the end"){
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(new Blockchain(), tiamatPublicKey)))

    blockchainActor ! StartMining
    retriesOnTimeout(1) {
      blockchainActor ! Request(1)
      p.receiveOne(500.millis)
    }
    blockchainActor ! Balance(tiamatPublicKey)
    val accountBalance = p.receiveOne(500.millis).asInstanceOf[Long]
    assert(accountBalance > 0)
    system.terminate()
  }

  test("can save and load blockchain") {
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(length3chain, tiamatPublicKey)))
    val length4chainActor = system.actorOf(Props(new BlockchainActor(length4chain, tiamatPublicKey)))

    val savePath = Paths.get("raicoin.chain")
    val saveDirPathName = savePath.toAbsolutePath.getParent.toString
    try {
      blockchainActor ! Request(3)
      p.expectNoMessage(100.millis)
      length4chainActor ! Save(saveDirPathName)
      p.expectNoMessage(100.millis)
      blockchainActor ! Load(saveDirPathName)
      blockchainActor ! Request(3)
      p.expectMsg(tcpWritten(length4chain(3)))
      Await.result(system.terminate(), Duration.Inf)
    } finally {
      Files.deleteIfExists(savePath)
    }
  }


}
