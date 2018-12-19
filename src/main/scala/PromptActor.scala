package raicoin

import akka.actor._
import scorex.crypto.signatures._
import org.apache.commons.codec.binary.Hex._
import scala.io.StdIn._
import scala.util.Try

class PromptActor(blockchainActorRef: ActorRef, publicKey: PublicKey, privateKey: PrivateKey)
    extends Actor {

  import context._

  def prompt(): Unit = self ! Option(readLine("> ")).getOrElse("exit").trim

  prompt()
  def receive = {
    case "save" => {
      blockchainActorRef ! Save(".")
      become(awaitingResponse)
    }

    case balance: String if balance.startsWith("balance") => {
      val tokens  = balance.split(" ")
      val account = Try(PublicKey(decodeHex(tokens(1)))).getOrElse(publicKey)
      blockchainActorRef ! GetBalance(account)
      become(awaitingResponse)
    }

    case "mining start" => {
      blockchainActorRef ! StartMining
      prompt()
    }
    case "mining stop" => {
      blockchainActorRef ! StopMining
      prompt()
    }

    case transfer: String if transfer.startsWith("send") => {
      val tokens    = transfer.split(" ")
      val recipient = PublicKey(decodeHex(tokens(1)))
      val amount    = tokens(2).toInt
      assert(Transaction(publicKey, recipient, amount).sign(privateKey).verify)
      blockchainActorRef ! Transaction(publicKey, recipient, amount).sign(privateKey)
      prompt()
    }
    case ""     => prompt()
    case "exit" => sys.exit(0)
    case _ => {
      println("Invalid command")
      prompt()
    }
  }

  def awaitingResponse: Receive =
    PartialFunction[Any, Unit] {
      case Balance(pubKey, balance) => {
        println(s"${encodeHexString(pubKey)}: $balance")
      }
      case Saved(x) => {
        println(s"Saved to $x")
      }
    }.andThen {
      case _ => {
        unbecome()
        prompt()
      }
    }

}
