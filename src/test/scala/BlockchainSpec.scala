package raicoin

import org.apache.commons.lang3.SerializationUtils._
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

  test("blockchain roundtrips through serialization") {
    val originals = Seq(rootOnly, length2chain, length3chain, length4chain)
    val roundtripped = originals.map(x => deserialize[Blockchain](serialize(x)))
    originals.zip(roundtripped).foreach{ case (orig, round) => assert(orig == round) }
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

}
