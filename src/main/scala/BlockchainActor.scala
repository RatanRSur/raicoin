import akka.actor._

case object Broadcast

class BlockchainActor(var blockchain: Blockchain) extends Actor {

  var unbroadcasted = blockchain.blocks

  def receive = {
    case Broadcast =>
      while (unbroadcasted.nonEmpty) {
        sender() ! unbroadcasted.head
        unbroadcasted = unbroadcasted.tail
      }
    case block: Block => {
      blockchain = blockchain :+ block
      unbroadcasted = (unbroadcasted :+ block).sorted(BlockOrdering)
    }
  }
}
