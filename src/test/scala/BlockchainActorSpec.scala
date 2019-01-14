package raicoin

import java.nio.file.{Files, Paths}

import akka.actor._
import akka.io.Tcp
import akka.testkit.TestProbe
import org.scalatest._
import raicoin.TestUtils._

import scala.concurrent.Await
import scala.concurrent.duration._

class BlockchainActorSpec extends FunSuiteLike {

  val timeout = 10.millis

  val noBindConfig = defaultConfig.copy(bind = false)

  test("adds received block to blockchain") {
    implicit val system        = ActorSystem()
    val blockchainActor        = system.actorOf(Props(new BlockchainActor(testChains(1))(noBindConfig)))
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor
    blockchainActor ! testChains(2).tip
    p.expectNoMessage(timeout)
    try {
      testChains(2).zipWithIndex.foreach {
        case (block, i) => {
          blockchainActor ! Request(i)
          expectTcpMessage(p, block)
        }
      }
    } finally {
      system.terminate()
    }
  }

  test("responds to request for one block") {
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(testChains(1))(noBindConfig)))

    blockchainActor ! Request(1)
    expectTcpMessage[MinedBlock](p, testChains(1).tip)
    system.terminate()
  }

  test("can keep track of longest chain") {
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(testChains(1))(noBindConfig)))
    val chainA = testChains(1)
      .mineBlock(Seq(testTransactions(1)), tiamatPublicKey)
    val chainB = testChains(1)
      .mineBlock(Seq(testTransactions(1)), tiamatPublicKey)
      .mineBlock(Seq(testTransactions(2)), tiamatPublicKey)

    try {
      blockchainActor ! chainA.tip
      blockchainActor ! Request(2)
      expectTcpMessage(p, chainA.tip)
      blockchainActor ! chainB(2)
      blockchainActor ! chainB(3)
      blockchainActor ! Request(2)
      expectTcpMessage(p, chainB(2))
    } finally {
      system.terminate()
    }
  }

  test("can handle blocks coming in in wrong order") {
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(testChains(0))(noBindConfig)))

    scala.util.Random.shuffle(0 to testChains(3).height - 1).foreach { i =>
      blockchainActor ! testChains(3)(i)
    }
    blockchainActor ! Request(3)
    try {
      expectTcpMessage(p, testChains(3)(3))
    } finally {
      system.terminate()
    }
  }

  test("mines block iff valid transaction received and is mining") {
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(testChains(1))(noBindConfig)))

    val transaction       = Transaction(tiamatPublicKey, vecnaPublicKey, 1)
    val signedTransaction = transaction.sign(tiamatPrivateKey)
    try {
      blockchainActor ! StartMining
      blockchainActor ! signedTransaction
      blockchainActor ! Request(2)
      assert(receiveOneTcpMessage[MinedBlock](p).transactions.contains(transaction))
      blockchainActor ! StopMining
      blockchainActor ! signedTransaction
      blockchainActor ! Request(3)
      assert(!receiveOneTcpMessage[MinedBlock](p).transactions.contains(transaction))
    } finally {
      system.terminate()
    }
  }

  test("does not mine block when invalid transaction received") {
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(testChains(1))(noBindConfig)))

    val transaction        = Transaction(tiamatPublicKey, vecnaPublicKey, 1)
    val invalidTransaction = transaction.sign(vecnaPrivateKey)
    try {
      blockchainActor ! StartMining
      blockchainActor ! invalidTransaction
      blockchainActor ! Request(2)
      p.expectNoMessage()
    } finally {
      system.terminate()
    }
  }

  test("does not remine transaction") {
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(testChains(1))(noBindConfig)))

    try {
      blockchainActor ! StartMining
      blockchainActor ! testTransactions(1)
      blockchainActor ! testTransactions(1)
      blockchainActor ! Transaction(tiamatPublicKey, vecnaPublicKey, 1).sign(tiamatPrivateKey)
      blockchainActor ! RequestBlocksSince(1)
      val msgs = p.receiveWhile(1.seconds, 500.millis) { case msg => msg }
      assert(
        msgs
          .filter(msg =>
            tcpUnwrap[MinedBlock](msg.asInstanceOf[Tcp.Write]).transactions
              .contains(testTransactions(1).transaction))
          .size === 1)
      blockchainActor ! GetBalance(vecnaPublicKey)
      p.expectMsg(Balance(vecnaPublicKey, 2))
    } finally {
      system.terminate()
    }
  }

  test("rejects block with invalid transactions") {
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(testChains(1))(noBindConfig)))

    val transaction        = Transaction(tiamatPublicKey, vecnaPublicKey, 1)
    val invalidTransaction = transaction.sign(vecnaPrivateKey)
    val invalidBlock =
      testChains(1).mineBlock(Seq(invalidTransaction), vecnaPublicKey).tip
    val validTransaction = transaction.sign(tiamatPrivateKey)
    val validBlock =
      testChains(1).mineBlock(Seq(validTransaction), vecnaPublicKey).tip

    try {
      blockchainActor ! invalidBlock
      blockchainActor ! Request(2)
      p.expectNoMessage(1.seconds)
      blockchainActor ! validBlock
      blockchainActor ! Request(2)
      p.receiveOne(500.millis)
    } finally {
      system.terminate()
    }
  }

  test("rejects block existing transaction") {
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(testChains(1))(noBindConfig)))

    val chainWithTransaction = testChains(1).mineBlock(Seq(testTransactions(0)), vecnaPublicKey)
    val chainWithDuplicateTransaction =
      chainWithTransaction.mineBlock(Seq(testTransactions(0)), vecnaPublicKey)

    val blockWithTransaction          = chainWithTransaction.tip
    val blockWithDuplicateTransaction = chainWithDuplicateTransaction.tip

    try {
      blockchainActor ! blockWithTransaction
      blockchainActor ! blockWithDuplicateTransaction
      blockchainActor ! Request(2)
      p.receiveOne(500.millis)
      blockchainActor ! Request(3)
      p.expectNoMessage(1.seconds)
    } finally {
      system.terminate()
    }
  }

  test("mining blockchain should have some value in account by the end") {
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(testChains(0))(noBindConfig)))

    try {
      blockchainActor ! StartMining
      retriesOnTimeout(1) {
        blockchainActor ! Request(1)
        p.receiveOne(500.millis)
      }
      blockchainActor ! GetBalance(tiamatPublicKey)
      val accountBalance = p.expectMsgType[Balance].balance
      assert(accountBalance > 0)
    } finally {
      system.terminate()
    }
  }

  test("can save and load blockchain with messages") {
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(testChains(2))(noBindConfig)))
    val length4chainActor =
      system.actorOf(Props(new BlockchainActor(testChains(3))(noBindConfig)))

    val savePath        = Paths.get("raicoin.chain")
    val saveDirPathName = savePath.toAbsolutePath.getParent.toString
    try {
      blockchainActor ! Request(3)
      p.expectNoMessage(100.millis)
      length4chainActor ! Save(saveDirPathName)
      p.expectMsg(Saved(savePath.toFile.getName))
      blockchainActor ! Load(saveDirPathName)
      blockchainActor ! Request(3)
      expectTcpMessage(p, testChains(3)(3))
      Await.result(system.terminate(), Duration.Inf)
    } finally {
      Files.deleteIfExists(savePath)
    }
  }

  test("can load blockchain using actor constructor") {
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val savingActor = system.actorOf(Props(new BlockchainActor(testChains(3))(noBindConfig)))

    val savePath        = Paths.get("raicoin.chain")
    val saveDirPathName = savePath.toAbsolutePath.getParent.toString
    try {
      savingActor ! Save(saveDirPathName)
      p.expectMsg(Saved(savePath.toFile.getName))
      val loadingActor = system.actorOf(Props(BlockchainActor.fromSavedBlockchain(savePath.toFile)))
      loadingActor ! Request(3)
      expectTcpMessage(p, testChains(3)(3))
      Await.result(system.terminate(), Duration.Inf)
    } finally {
      Files.deleteIfExists(savePath)
    }
  }

  test("can save \"big\" blockchain") {
    implicit val system        = ActorSystem()
    val p                      = TestProbe("p")(system)
    implicit val defaultSender = p.testActor

    val blockchainActor = system.actorOf(Props(new BlockchainActor(testChains(0))(noBindConfig)))

    val savePath        = Paths.get("raicoin.chain")
    val saveDirPathName = savePath.toAbsolutePath.getParent.toString
    blockchainActor ! StartMining
    def loopUntilBig(): Unit = {
      blockchainActor ! Height
      if (p.receiveOne(100.millis).asInstanceOf[Int] > 1000) ()
      else {
        Thread.sleep(100)
        loopUntilBig()
      }
    }
    loopUntilBig()
    blockchainActor ! StopMining
    try {
      blockchainActor ! Save(saveDirPathName)
      p.expectMsg(Saved(savePath.toFile.getName))
    } finally {
      Await.result(system.terminate(), Duration.Inf)
      Files.deleteIfExists(savePath)
    }
  }
}
