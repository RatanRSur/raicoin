package raicoin

import org.scalatest._
import raicoin.Exceptions._
import raicoin.TestUtils._
import scorex.crypto.signatures._

import scala.util.Random

class BlockchainSpec extends FunSuite {

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

  test("add block onto blockchain") {
    assert(length2chain.height === 2)
    assert(length2chain.ledger(vecnaPublicKey) === 0)
    assert(length2chain.ledger(tiamatPublicKey) === 1)
  }

  test("can't send more tokens than you have") {
    val transactions = Seq(Transaction(vecnaPublicKey, tiamatPublicKey, 5),
                           Transaction(tiamatPublicKey, vecnaPublicKey, 5)).map { tx =>
      val keyToSignWith =
        if (tx.sender == tiamatPublicKey) tiamatPrivateKey else vecnaPrivateKey
      tx.sign(keyToSignWith)
    }
    assertThrows[IllegalTransactions] {
      rootOnly.mineBlock(transactions, vecnaPublicKey)
    }
  }

  test("can't send yourself tokens") {
    assertThrows[IllegalTransactions] {
      Transaction(vecnaPublicKey, vecnaPublicKey, 1)
    }
  }

  test("hash of mined blocks begin with 0's according to difficulty") {
    assert(length4chain.tail.map(_.hash).forall(_.startsWith(Seq.fill(length2chain.difficulty)(0))),
           length4chain.toString)
  }

  test("ledger access determined by value of public key, not object") {
    assert(
      length4chain.ledger(tiamatPublicKey) ===
        length4chain.ledger(PublicKey(tiamatPublicKey.clone())))
  }

  //test("blockchain hash chain is solid all the way back") {
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
