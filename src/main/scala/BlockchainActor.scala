package ratan.blockchain

import akka.actor._

case object Broadcast
case class Request(index: Int)

class BlockchainActor(var blockchain: Blockchain) extends Actor {

  def receive = {
    case block: MinedBlock =>
      if (block.index <= blockchain.height)
        blockchain = blockchain.append(block)
    case Request(index) => {
      if (index < blockchain.height) sender() ! blockchain(index)
    }
  }
}
