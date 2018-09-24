package raicoin

import akka.actor._

import akka.io.Tcp
import akka.io.IO
import java.net.InetAddress
import java.net.InetSocketAddress
import scala.concurrent.duration._
import akka.util.ByteString
import Serializer._
import org.apache.commons.codec.binary.Hex

case object Broadcast
case class Request(index: Int)
case class RequestBlocksSince(index: Int)

object BlockchainActor {
  val BootstrapNodeAddress = new InetSocketAddress("localhost", 6364)
  val BindAddress          = new InetSocketAddress("localhost", 6363)
}

class BlockchainActor(var blockchain: Blockchain,
                      val startingPeer: Option[InetSocketAddress] = Some(
                        BlockchainActor.BootstrapNodeAddress))
    extends Actor {
  import context._
  import BlockchainActor._

  var peer: Option[ActorRef] = None
  var orphans                = Seq[MinedBlock]()

  locally {
    val tcpManager = IO(Tcp)
    startingPeer match {
      case Some(peer) =>
        tcpManager ! Tcp.Connect(peer, Some(BindAddress))
      case None =>
        tcpManager ! Tcp.Bind(self, BootstrapNodeAddress)
    }
  }

  def receive = {
    case block: MinedBlock => {
      println(s"receiving $block from $sender")
      // new* assignments to get around scala limitations of multiple assignment
      val (newBlockchain, newOrphans) =
        blockchain.resolveOrphans(orphans :+ block)
      blockchain = newBlockchain
      orphans = newOrphans
    }
    case RequestBlocksSince(index) => {
      (index until blockchain.height).map(i => blockchain(i)).foreach { block =>
        {
          println(s"sending $block to $sender")
          sender() ! Tcp.Write(serialize(block))
        }
      }
    }
    case r @ Request(index) => {
      if (index < blockchain.height) {
        sender() ! Tcp.Write(serialize(blockchain(index)))
      }
    }
    case _: Tcp.Bound => // do nothing
    case Tcp.Connected(remote, local) => {
      val connection = sender()
      peer = Some(connection)
      connection ! Tcp.Register(context.self)
      system.scheduler.schedule(0.millis, 100.millis) {
        connection ! Tcp.Write(serialize(RequestBlocksSince(blockchain.height)))
      }
    }
    case Tcp.Received(data) => {
      self.!(deserialize(data))(sender())
    }
    case other => {
      println(s"Unexpected Message: ${context.system}: $other")
    }
  }

}
