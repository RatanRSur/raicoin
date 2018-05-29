import akka.actor._

case object Broadcast
case class Received(index: Long, hashHex: String)

class BlockchainActor(var blockchain: Blockchain) extends Actor {

  var unbroadcasted = blockchain.blocks

  def receive = {
    case Broadcast =>
      while (unbroadcasted.nonEmpty) {
        sender() ! unbroadcasted.head
        unbroadcasted = unbroadcasted.tail
      }
    case block: Block => sender() ! Received(block.index, block.hashHex)
  }
}
