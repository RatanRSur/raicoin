import akka.actor._

case object Broadcast
case class Request(index: Int)

class BlockchainActor(var blockchain: Blockchain) extends Actor {

  var floatingBlocks = Seq[Block]()

  def receive = {
    case block: Block => {
      if (block.index > blockchain.length) {
        floatingBlocks = (floatingBlocks :+ block).sorted(BlockOrdering)
      } else {
        blockchain = blockchain :+ block
        while (floatingBlocks.nonEmpty && floatingBlocks.head.index == blockchain.length) {
          blockchain = blockchain :+ floatingBlocks.head
          floatingBlocks = floatingBlocks.tail
        }
      }
    }
    case Request(index) => {
      sender() ! blockchain(index)
    }
  }
}
