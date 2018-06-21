import org.scalatest._
import scala.util.Random
import java.security.MessageDigest
import Exceptions._

class BlockchainSpec extends FunSuite with TestChains {

  def randomUserName = Random.nextString(5)

  def random[T](s: Iterable[T]): T = {
    val n = Random.nextInt(s.size)
    s.iterator.drop(n).next
  }

  test("initial Blockchain no blocks and no users") {
    assert(rootOnly.height === 1)
    assert(rootOnly.tip.ledger.size === 0)
  }

  test("add users") {
    assert(length2chain.height === 2)
    assert(length2chain.tip.ledger.size === 2)
  }

  test("can't add user that already exists") {
    assertThrows[UserAlreadyExists] {
      length2chain.mineBlock(Seq(Transaction("vecna", "tiamat", 1)), "vecna", Seq("vecna"))
    }
  }

  test("add block onto blockchain") {
    assert(length2chain.height === 2)
    assert(length2chain.tip.ledger("vecna") === 0)
    assert(length2chain.tip.ledger("tiamat") === 1)
  }

  test("can't send more tokens than you have") {
    val transactions = Seq(Transaction("vecna", "tiamat", 5), Transaction("tiamat", "vecna", 5))
    assertThrows[IllegalTransactions] {
      rootOnly.mineBlock(transactions, "vecna", Seq("vecna", "tiamat"))
    }
  }

  test("can't send yourself tokens") {
    assertThrows[IllegalTransactions] {
      Transaction("vecna", "vecna", 1)
    }
  }

  test("every block must have at least 1 transaction") {
    assertThrows[IllegalTransactions] {
      rootOnly.mineBlock(Seq(), "vecna", Seq("vecna"))
    }
  }

  test("hash of mined blocks begin with 0's according to difficulty") {
    assert(length4chain.tail.map(_.hash).forall(_.startsWith(Seq.fill(length2chain.difficulty)(0))),
           length4chain.toString)
  }

  test("blockchain hash chain is solid all the way back") {
    val randomUserNames = Seq.fill(10)(randomUserName).toSet
    val randomUserNames = Seq.fill(10)(randomUserName)
    val testLedger = (new Ledger() /: randomUserNames) { (ledger, user) =>
      ledger + (user -> 50000000000L)
    }
    val randomTransactions = (0 until 10).map { _ =>
      val sender    = random(randomUserNames)
      val recipient = random(randomUserNames - sender)
      Transaction(sender, recipient, 1)
    }
    val chain = new Blockchain(Seq(new RootBlock(testLedger)))
    val newChain = (chain /: randomTransactions) { (blockchain, transaction) =>
      blockchain.mineBlock(Seq(transaction), random(randomUserNames))
    }

    def testBlock(mb: MinedBlock): Unit = {
      val manualPrevBlockHash = {
        val sha          = MessageDigest.getInstance("SHA-256")
        val ledger       = new SHAHashable { val hashDependencies = Seq(mb.ledger.hash) }
        val transactions = new SHAHashable { val hashDependencies = Seq(mb.transactions.hash) }
        sha.update(mb.previousBlock.hash)
        Seq[SHAHashable](ledger, transactions).foreach(x => sha.update(x.hash))
        sha.digest
      }
      assert(mb.hash == manualPrevBlockHash)
      mb.previousBlock match {
        case alsoMB: MinedBlock => testBlock(alsoMB)
        case _                  => ()
      }
    }
  }
}
