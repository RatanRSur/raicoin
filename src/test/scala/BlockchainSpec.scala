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
    assert(testChains(0).height === 1)
    assert(testChains(0).ledger.size === 0)
  }

  test("add users") {
    assert(testChains(1).height === 2)
    assert(testChains(1).ledger.size === 2)
  }

  test("add block onto blockchain") {
    assert(testChains(1).height === 2)
    assert(testChains(1).ledger(vecnaPublicKey) === 0)
    assert(testChains(1).ledger(tiamatPublicKey) === 1)
  }

  test("can't send more tokens than you have") {
    val transactions = Seq(Transaction(vecnaPublicKey, tiamatPublicKey, 5),
                           Transaction(tiamatPublicKey, vecnaPublicKey, 5)).map { tx =>
      val keyToSignWith =
        if (tx.sender == tiamatPublicKey) tiamatPrivateKey else vecnaPrivateKey
      tx.sign(keyToSignWith)
    }
    assertThrows[IllegalTransactions] {
      testChains(0).mineBlock(transactions, vecnaPublicKey)
    }
  }

  test("can't send yourself tokens") {
    assertThrows[IllegalTransactions] {
      Transaction(vecnaPublicKey, vecnaPublicKey, 1)
    }
  }

  test("blockchain roundtrips through serialization") {
    val originals    = Seq(testChains(0), testChains(1), testChains(2), testChains(3))
    val roundtripped = originals.map(x => deserialize[Blockchain](serialize(x)))
    originals.zip(roundtripped).foreach { case (orig, round) => assert(orig == round) }
  }

  test("hash of mined blocks begin with 0's according to difficulty") {
    assert(
      testChains(3).tail.map(_.hash).forall(_.startsWith(Seq.fill(testChains(1).difficulty)(0))),
      testChains(3).toString)
  }

  test("ledger access determined by value of public key, not object") {
    assert(
      testChains(3).ledger(tiamatPublicKey) ===
        testChains(3).ledger(PublicKey(tiamatPublicKey.clone())))
  }

}
