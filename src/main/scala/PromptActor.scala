package raicoin

import akka.actor._
import scorex.crypto.signatures._
import org.apache.commons.codec.binary.Hex._
import scala.io.StdIn._
import scala.util.Try

class PromptActor(blockchainActorRef: ActorRef)(implicit config: Config) extends Actor {

  import context._

  blockchainActorRef ! RegisterPrompt

  def prompt(): Unit = {
    if (config.interactive) self ! Option(readLine("> ")).getOrElse("exit").trim
    else ()
  }

  prompt()

  def receive = {
    case "save" => {
      blockchainActorRef ! Save(".")
      become(awaitingResponse)
    }

    case balance: String if balance.startsWith("balance") => {
      val tokens = balance.split(" ")
      try {
        val account = if (tokens.length > 1) {
          PublicKey(decodeHex(tokens(1)))
        } else {
          config.publicKey
        }
        blockchainActorRef ! GetBalance(account)
        become(awaitingResponse)
      } catch {
        case de: org.apache.commons.codec.DecoderException =>
          println(s"Invalid public key. ${de.getMessage}")
          prompt()
      }
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
      val tokens = transfer.split(" ")
      try {
        val recipient = PublicKey(decodeHex(tokens(1)))
        val amount    = tokens(2).toInt
        assert(Transaction(config.publicKey, recipient, amount).sign(config.privateKey).verify)
        blockchainActorRef ! Transaction(config.publicKey, recipient, amount)
          .sign(config.privateKey)
      } catch {
        case ioobe: IndexOutOfBoundsException => println("Invalid input.\nsend PUBLICKEY AMOUNT")
      }
      prompt()
    }
    case ""                                    => prompt()
    case "exit"                                => sys.exit(0)
    case str if sender() == blockchainActorRef => println(str)
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
