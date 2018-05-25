import scala.collection.SortedSet

class Blockchain(private val blocks: Seq[Block] = Seq(new RootBlock())) extends Seq[Block] {

  private implicit def toChain(seq: Seq[Block]): Blockchain = new Blockchain(seq)

  // Members declared in IterableLike
  def iterator: Iterator[Block] = blocks.iterator

  // Members declared in SeqLike
  def apply(idx: Int): Block = blocks(idx)
  def length: Int = blocks.length

  def mineBlock(transactions: Seq[Transaction], miner: String, newUsers: Seq[String]=Seq.empty): Blockchain = {
    this :+ new MinedBlock(this.last, transactions, miner, newUsers)
  }

}
