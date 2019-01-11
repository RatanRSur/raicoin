package raicoin

import java.io.File
import java.net.InetSocketAddress
import java.nio.file.Paths

import akka.actor._
import akka.io.{IO, Tcp}
import org.apache.commons.io.FileUtils
import org.apache.commons.lang3.SerializationUtils.{deserialize, serialize}
import raicoin.Serializer._
import scorex.crypto.signatures._

import scala.concurrent.duration._

case object Broadcast
case class Request(index: Int)
case class RequestBlocksSince(index: Int)
case object GetPeers
case object GetPeerInfo
case object Disconnect
case class Save(directoryName: String)
case class Saved(fileName: String)
case class Load(directoryName: String)
case class GetBalance(publicKey: PublicKey)
case class Balance(publicKey: PublicKey, balance: Long)
case object StartMining
case object StopMining
case object MineEmptyBlockIfIdle
case object Height
case object RegisterPrompt

object SerializableInetSocketAddressImplicit {
  implicit class SerializableINSA(insa: InetSocketAddress)
      extends InetSocketAddress(insa.getAddress, insa.getPort)
      with Serializable
}

object BlockchainActor {
  def fromSavedBlockchain(blockchainFile: File)(implicit config: Config): BlockchainActor = {
    new BlockchainActor(deserialize(FileUtils.readFileToByteArray(blockchainFile)))
  }
}

class BlockchainActor(var blockchain: Blockchain)(implicit config: Config) extends Actor {

  import SerializableInetSocketAddressImplicit._
  import context._

  var mySocketAddress: Option[InetSocketAddress] = None
  var knownPeers                                 = config.startingPeers.toSet
  var connectedPeers                             = Map[ActorRef, InetSocketAddress]()

  var orphans          = Seq[MinedBlock]()
  var seenTransactions = Set.empty[Transaction]

  var myListeningAddress: Option[InetSocketAddress] = None

  val tcpManager = IO(Tcp)

  var promptActor: Option[ActorRef] = None
  def logToPrompt(str: String): Unit = {
    promptActor.foreach(_ ! str)
  }

  tcpManager ! Tcp.Bind(self, config.listeningSocketAddress)

  def mineSignedTransaction(st: SignedTransaction) = {
    if (!seenTransactions.contains(st.transaction) && st.verify) {
      blockchain = blockchain.mineBlock(Seq(st), config.publicKey)
      seenTransactions += st.transaction
    }
  }

  def idleMining: Receive = {
    case st: SignedTransaction => {
      mineSignedTransaction(st)
      become(mining.orElse(receive))
    }
    case MineEmptyBlockIfIdle => {
      blockchain = blockchain.mineBlock(Seq(), config.publicKey)
      self ! MineEmptyBlockIfIdle
    }
  }

  def mining: Receive = {
    case st: SignedTransaction => {
      mineSignedTransaction(st)
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
    case st: SignedTransaction => {
      if (!seenTransactions.contains(st.transaction)) {
        connectedPeers.keys.foreach(_ ! Tcp.Write(toByteString(st)))
      }
    }
    case block: MinedBlock => {
      // new* assignments to get around scala limitations of multiple assignment
      if (block.signedTransactions.forall { st =>
            !seenTransactions.contains(st.transaction) &&
            st.verify
          }) {

        val (newBlockchain, newOrphans) = blockchain.resolveOrphans(orphans :+ block)
        blockchain = newBlockchain
        orphans = newOrphans
        block.transactions.foreach(seenTransactions += _)
      }
    }
    case RequestBlocksSince(index) => {
      (index until blockchain.height).map(i => blockchain(i)).foreach { block =>
        sender() ! Tcp.Write(toByteString(block))
      }
    }
    case Request(index) => {
      if (index < blockchain.height) {
        sender() ! Tcp.Write(toByteString(blockchain(index)))
      }
    }
    case Tcp.Bound(insa) => {
      logToPrompt(s"bound to $insa")
      mySocketAddress = Some(insa)
      knownPeers.foreach { kp: InetSocketAddress =>
        {
          logToPrompt(s"trying to connect to $kp")
          tcpManager ! Tcp.Connect(kp)
        }
      }
    }
    case Tcp.Connected(remoteAddress, _) => {
      logToPrompt(s"connected to $remoteAddress")
      val peerRef = sender()

      peerRef ! Tcp.Register(context.self)

      connectedPeers += (peerRef -> remoteAddress)
      peerRef ! Tcp.Write(toByteString(GetPeerInfo))

      system.scheduler.schedule(0.millis, 500.millis) {
        peerRef ! Tcp.Write(toByteString(RequestBlocksSince(blockchain.height)))
      }
    }
    case GetPeerInfo => {
      mySocketAddress.foreach { insa =>
        sender() ! Tcp.Write(toByteString(insa))
      }
    }
    case Tcp.Received(data) => {
      //println(s"${system.name}: ${fromByteString(data)}")
      self.!(fromByteString(data))(sender())
    }
    case GetPeers => {
      knownPeers.foreach { insa =>
        {
          sender() ! Tcp.Write(toByteString(insa))
        }
      }
    }
    case insa: InetSocketAddress => {
      if (!mySocketAddress.contains(insa) && !knownPeers.contains(insa)) {
        logToPrompt(s"new peer: $insa")
        knownPeers += insa
        tcpManager ! Tcp.Connect(insa)
      }
    }
    case Disconnect => {
      connectedPeers.keys.foreach { connection =>
        connection ! Tcp.ConfirmedClose
      }
    }
    case Save(directoryName) => {
      val chainFile = new File(directoryName, "raicoin.chain")
      FileUtils.writeByteArrayToFile(chainFile, serialize(blockchain))
      sender() ! Saved(chainFile.getName)
    }
    case Load(directoryName) => {
      blockchain = deserialize(
        FileUtils.readFileToByteArray(new File(directoryName, "raicoin.chain")))
    }
    case GetBalance(publicKey) => sender() ! Balance(publicKey, blockchain.ledger(publicKey))
    case Height                => sender() ! blockchain.height
    case RegisterPrompt => {
      val pa = sender()
      promptActor = Some(pa)
    }
    case other => {
      //println(s"Unhandled Message: ${context.system}: $other")
    }
  }
}
