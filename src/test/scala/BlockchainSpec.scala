import org.scalatest._

class BlockchainSpec extends FunSuite {

  test("initial Blockchain no blocks and no users") {
    val chain = new Blockchain()
    assert(chain.size === 1)
    assert(chain.last.ledger.size === 0)
  }

  test("add users") {
    val chain =
      new Blockchain().mineBlock(Seq.empty, "vecna", Set("vecna", "tiamat"))
    assert(chain.size === 2)
    assert(chain.last.ledger.size === 2)
  }

  test("add block onto blockchain") {
    val chain = new Blockchain()
    val transactions = Seq(Transaction("vecna", "tiamat", 1))
    val newChain = chain.mineBlock(transactions, "vecna", Set("vecna", "tiamat"))
    assert(newChain.size === 2)
    assert(newChain.last.ledger("vecna").balance === 0)
    assert(newChain.last.ledger("tiamat").balance === 1)
  }
}
