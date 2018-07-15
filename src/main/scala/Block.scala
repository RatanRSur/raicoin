package ratan.blockchain

import scala.util.Random
import scala.collection.SortedSet
import java.security.MessageDigest
import java.nio.ByteBuffer
import scala.util.{Try, Success, Failure}

object BlockOrdering extends Ordering[Block] {
  def compare(a: Block, b: Block) = -a.index.compare(b.index)
}

abstract class Block extends SHAHashable {
  val index: Int
  val ledger: Ledger
  override lazy val toString: String = s"${getClass.getName}(index: $index, hash: $hashHex)"
}

case class RootBlock(val ledger: Ledger = new Ledger()) extends Block {
  val index            = 0
  val hashDependencies = Seq(ledger.hash)
}

case class MinedBlock(val previousBlock: Block,
                      val transactions: Seq[Transaction],
                      val miner: String,
                      val newUsers: Seq[String],
                      difficulty: Int)
    extends Block {
  val index = previousBlock.index + 1

  val ledger =
    previousBlock.ledger
      .addUsers(newUsers)
      .rewardMiner(miner)
      .applyTransactions(transactions)
      .get

  val hashDependencies = Seq[SHAHashable](previousBlock, transactions, ledger).map(_.hash)

  override lazy val hash = {
    var currentNonce = 0
    def currentHash = {
      sha.reset()
      hashDependencies.foreach(sha.update)
      val buf = ByteBuffer.allocate(4).putInt(currentNonce)
      sha.update(buf.array)
      sha.digest
    }
    while (!currentHash.startsWith(Seq.fill(difficulty)(0))) {
      currentNonce += 1
    }
    currentHash
  }

}
