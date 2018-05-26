import org.scalatest._
import scala.util.Random
import java.security.MessageDigest

class BlockchainSpec extends FunSuite {

  def randomUserName = Random.nextString(5)

  def random[T](s: Iterable[T]): T = {
    val n = Random.nextInt(s.size)
    s.iterator.drop(n).next
  }

  test("initial Blockchain no blocks and no users") {
    val chain = new Blockchain()
    assert(chain.size === 1)
    assert(chain.last.ledger.size === 0)
  }

  test("add users") {
    val chain =
      new Blockchain().mineBlock(Seq.empty, "vecna", Seq("vecna", "tiamat"))
    assert(chain.size === 2)
    assert(chain.last.ledger.size === 2)
  }

  test("can't add user that already exists") {
    val chain =
      new Blockchain().mineBlock(Seq.empty, "vecna", Seq("vecna", "tiamat"))
    assertThrows[IllegalArgumentException] {
      chain.mineBlock(Seq.empty, "vecna", Seq("vecna"))
    }
  }

  test("add block onto blockchain") {
    val chain        = new Blockchain()
    val transactions = Seq(Transaction("vecna", "tiamat", 1))
    val newChain     = chain.mineBlock(transactions, "vecna", Seq("vecna", "tiamat"))
    assert(newChain.size === 2)
    assert(newChain.last.ledger("vecna") === 0)
    assert(newChain.last.ledger("tiamat") === 1)
  }

  test("can't send more tokens than you have") {
    val transactions = Seq(Transaction("vecna", "tiamat", 1), Transaction("vecna", "tiamat", 6))
    val newChain     = new Blockchain().mineBlock(transactions, "vecna", Seq("vecna", "tiamat"))
    assert(!newChain.last.isValid)
  }

  test("can't send yourself tokens") {
    val chain        = new Blockchain()
    val transactions = Seq(Transaction("vecna", "vecna", 1))
    val newChain     = chain.mineBlock(transactions, "vecna", Seq("vecna"))
    assert(!newChain.last.isValid)
  }

  test("every block must have at least 1 transaction") {
    val chain        = new Blockchain()
    val transactions = Seq()
    val newChain     = chain.mineBlock(transactions, "vecna", Seq("vecna"))
    assert(!newChain.last.isValid)
  }

  test("blockchain hash chain is solid all the way back") {
    val randomUserNames = Seq.fill(10)(randomUserName).toSet
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
        val timestamp    = new SHAHashable { val hashDependencies = Seq(mb.timestamp.hash) }
        val ledger       = new SHAHashable { val hashDependencies = Seq(mb.ledger.hash) }
        val transactions = new SHAHashable { val hashDependencies = Seq(mb.transactions.hash) }
        sha.update(mb.previousBlock.hash)
        Seq[SHAHashable](timestamp, ledger, transactions).foreach(x => sha.update(x.hash))
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
