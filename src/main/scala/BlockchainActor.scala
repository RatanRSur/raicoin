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
import java.util.UUID._
import java.io.File
import scorex.crypto.signatures._
import org.apache.commons.lang3.SerializationUtils.{serialize, deserialize}
import org.apache.commons.io.FileUtils

case object Broadcast
case class Request(index: Int)
case class RequestBlocksSince(index: Int)
case object GetPeers
case class PeerInfo(id: String, hostname: String, port: Int) {
  def inetSocketAddress = new InetSocketAddress(hostname, port)
}
case object PeerInfo {
  def fromInetSocketAddress(id: String, insa: InetSocketAddress): PeerInfo = {
    PeerInfo(id, insa.getHostName, insa.getPort)
  }
}
case object GetPeerInfo
case object Disconnect
case class Save(directoryName: String)
case class Saved(fileName: String)
case class Load(directoryName: String)
case class Balance(publicKey: PublicKey)
case object StartMining
case object StopMining
case object MineEmptyBlockIfIdle
case object Height

object BlockchainActor {
  val BootstrapPeerInfo = PeerInfo("55f119f3-c33b-4078-92e5-923f6cd200f2", "localhost", 6364)
}

class BlockchainActor(var blockchain: Blockchain,
                      publicKey: PublicKey,
                      val startingPeer: Option[PeerInfo] = Some(BlockchainActor.BootstrapPeerInfo))
    extends Actor {

  import context._
  import BlockchainActor._

  val id                           = randomUUID().toString
  var myPeerInfo: Option[PeerInfo] = None
  var knownPeers                   = Set.empty[PeerInfo]
  var connectedPeers               = Map[ActorRef, Option[PeerInfo]]()

  var orphans = Seq[MinedBlock]()

  var myListeningAddress: Option[InetSocketAddress] = None

  val tcpManager = IO(Tcp)

  startingPeer match {
    case Some(sp) => {
      knownPeers += sp
      tcpManager ! Tcp.Bind(self, new InetSocketAddress("localhost", 0))
    }
    case None => tcpManager ! Tcp.Bind(self, BootstrapPeerInfo.inetSocketAddress)
  }

  def idleMining: Receive = {
    case st: SignedTransaction => {
      become(mining.orElse(receive))
      self.!(st)(sender())
    }
    case MineEmptyBlockIfIdle => {
      blockchain = blockchain.mineBlock(Seq(), publicKey)
      self ! MineEmptyBlockIfIdle
    }
  }

  def mining: Receive = {
    case st: SignedTransaction => {
      if (st.verify) {
        blockchain = blockchain.mineBlock(Seq(st), publicKey)
      }
    }
    case MineEmptyBlockIfIdle => {
      become(idleMining.orElse(receive))
      self ! MineEmptyBlockIfIdle
    }
  }

  def receive: Receive = {
    case StartMining => {
      become(mining.orElse(receive))
      self ! MineEmptyBlockIfIdle
    }
    case StopMining => become(receive)
    case block: MinedBlock => {
      // new* assignments to get around scala limitations of multiple assignment
      //println(s"${system.name} $blockchain")
      if (block.signedTransactions.forall(_.verify)) {

        val (newBlockchain, newOrphans) = blockchain.resolveOrphans(orphans :+ block)
        blockchain = newBlockchain
        orphans = newOrphans

      }
      //println(s"${system.name} $blockchain")
    }
    case RequestBlocksSince(index) => {
      (index until blockchain.height).map(i => blockchain(i)).foreach {
        block => sender() ! Tcp.Write(toByteString(block))
      }
    }
    case Request(index) => {
      if (index < blockchain.height) {
        sender() ! Tcp.Write(toByteString(blockchain(index)))
      }
    }
    case Tcp.Bound(address) => {
      myPeerInfo = Some(PeerInfo.fromInetSocketAddress(id, address))
      knownPeers.foreach { kp: PeerInfo =>
        {
          tcpManager ! Tcp.Connect(kp.inetSocketAddress)
        }
      }
    }
    case _: Tcp.Connected => {
      val peerRef = sender()

      peerRef ! Tcp.Register(context.self)

      connectedPeers += (peerRef -> None)
      peerRef ! Tcp.Write(toByteString(GetPeerInfo))

      system.scheduler.schedule(0.millis, 500.millis) {
        peerRef ! Tcp.Write(toByteString(RequestBlocksSince(blockchain.height)))
      }
    }
    case GetPeerInfo => {
      myPeerInfo.foreach { pi =>
        sender() ! Tcp.Write(toByteString(pi))
      }
    }
    case Tcp.Received(data) => {
      //println(s"${system.name}: ${fromByteString(data)}")
      self.!(fromByteString(data))(sender())
    }
    case GetPeers => {
      knownPeers.foreach { peerInfo =>
        {
          sender() ! Tcp.Write(toByteString(peerInfo))
        }
      }
    }
    case pi: PeerInfo => {
      val currentConnection = sender()
      if (!myPeerInfo.contains(pi)) {
        if (connectedPeers(currentConnection).isEmpty) {
          knownPeers += pi
          connectedPeers += (currentConnection -> Some(pi))
          currentConnection ! Tcp.Write(toByteString(GetPeers))
        } else if (!knownPeers.contains(pi)) {
          knownPeers += pi
          tcpManager ! Tcp.Connect(pi.inetSocketAddress)
        }
      }
    }
    case Disconnect => {
      connectedPeers.keys.foreach { connection =>
        connection ! Tcp.ConfirmedClose
      }
    }
    case Save(directoryName) => {
      val chainFile = new File(directoryName, "raicoin.chain")
      FileUtils.writeByteArrayToFile(
        chainFile,
        serialize(blockchain))
      sender() ! Saved(chainFile.getName)
    }
    case Load(directoryName) => {
      blockchain = deserialize(
        FileUtils.readFileToByteArray(new File(directoryName, "raicoin.chain")))
    }
    case Balance(publicKey) => sender() ! blockchain.ledger(publicKey)
    case Height => sender() ! blockchain.height
    case other => {
      println(s"Unhandled Message: ${context.system}: $other")
    }
  }
}
