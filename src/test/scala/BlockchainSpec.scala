import org.scalatest._
import scala.util.Random

class BlockchainSpec extends FunSuite {

  def randomUserName = Random.nextString(5)

  //def random[T](s: TraversableOnce[T]): T = {
    //val n = Random.nextInt(s.size)
    //s.iterator.drop(n).next
  //}

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

  test("can't send more tokens than you have") {
    val transactions = Seq(
      Transaction("vecna", "tiamat", 1),
      Transaction("vecna", "tiamat", 6))
    val newChain = new Blockchain().mineBlock(transactions, "vecna", Set("vecna", "tiamat"))
    assert(!newChain.last.isValid)
  }

  test("can't send yourself tokens") {
    val chain = new Blockchain()
    val transactions = Seq(Transaction("vecna", "vecna", 1))
    val newChain = chain.mineBlock(transactions, "vecna", Set("vecna"))
    assert(!newChain.last.isValid)
  }

  test("every block must have at least 1 transaction") {
    val chain = new Blockchain()
    val transactions = Seq()
    val newChain = chain.mineBlock(transactions, "vecna", Set("vecna"))
    assert(!newChain.last.isValid)
  }
}
