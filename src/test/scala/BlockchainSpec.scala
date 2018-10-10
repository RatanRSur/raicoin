package raicoin

import org.scalatest._
import scala.util.Random
import java.security.MessageDigest
import Exceptions._
import TestUtils._

class BlockchainSpec extends FunSuite with TestChains {

  def randomUserName = Random.nextString(5)

  def random[T](s: Iterable[T]): T = {
    val n = Random.nextInt(s.size)
    s.iterator.drop(n).next
  }

  test("initial Blockchain no blocks and no users") {
    assert(rootOnly.height === 1)
    assert(rootOnly.ledger.size === 0)
  }

  test("add users") {
    assert(length2chain.height === 2)
    assert(length2chain.ledger.size === 2)
  }

  test("can't add user that already exists") {
    assertThrows[PublicKeyAlreadyExists] {
      length2chain.mineBlock(Seq(Transaction(vecnaPublicKey, tiamatPublicKey, 1)), vecnaPublicKey, Seq(vecnaPublicKey))
    }
  }

  test("add block onto blockchain") {
    assert(length2chain.height === 2)
    assert(length2chain.ledger(vecnaPublicKey) === 0)
    assert(length2chain.ledger(tiamatPublicKey) === 1)
  }

  test("can't send more tokens than you have") {
    val transactions = Seq(Transaction(vecnaPublicKey, tiamatPublicKey, 5), Transaction(tiamatPublicKey, vecnaPublicKey, 5))
    assertThrows[IllegalTransactions] {
      rootOnly.mineBlock(transactions, vecnaPublicKey, Seq(vecnaPublicKey, tiamatPublicKey))
    }
  }

  test("can't send yourself tokens") {
    assertThrows[IllegalTransactions] {
      Transaction(vecnaPublicKey, vecnaPublicKey, 1)
    }
  }

  test("every block must have at least 1 transaction") {
    assertThrows[IllegalTransactions] {
      rootOnly.mineBlock(Seq(), vecnaPublicKey, Seq(vecnaPublicKey))
    }
  }

  test("hash of mined blocks begin with 0's according to difficulty") {
    assert(length4chain.tail.map(_.hash).forall(_.startsWith(Seq.fill(length2chain.difficulty)(0))),
           length4chain.toString)
  }

  //ignore("blockchain hash chain is solid all the way back") {
  //val randomUserNames = Seq.fill(10)(randomUserName).toSet
  //val testLedger = (new Ledger() /: randomUserNames) { (ledger, user) =>
  //ledger + (user -> 50000000000L)
  //}
  //val randomTransactions = (0 until 10).map { _ =>
  //val sender    = random(randomUserNames)
  //val recipient = random(randomUserNames - sender)
  //Transaction(sender, recipient, 1)
  //}
  //val chain = new Blockchain(Seq(new RootBlock(testLedger)))
  //val newChain = (chain /: randomTransactions) { (blockchain, transaction) =>
  //blockchain.mineBlock(Seq(transaction), random(randomUserNames))
  //}

  //def testBlock(mb: MinedBlock): Unit = {
  //val manualPrevBlockHash = {
  //val sha          = MessageDigest.getInstance("SHA-256")
  //val ledger       = new SHAHashable { val hashDependencies = Seq(mb.ledger.hash) }
  //val transactions = new SHAHashable { val hashDependencies = Seq(mb.transactions.hash) }
  //sha.update(mb.previousBlock.hash)
  //Seq[SHAHashable](ledger, transactions).foreach(x => sha.update(x.hash))
  //sha.digest
  //}
  //assert(mb.hash == manualPrevBlockHash)
  //mb.previousBlock match {
  //case alsoMB: MinedBlock => testBlock(alsoMB)
  //case _                  => ()
  //}
  //}
  //}
}
