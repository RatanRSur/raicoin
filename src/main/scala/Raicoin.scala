package raicoin

import scala.io.StdIn._
import scorex.crypto._
import scorex.crypto.signatures._
import org.apache.commons.codec.binary.Hex._
import org.apache.commons.io.FileUtils
import org.apache.commons.io.FilenameUtils
import java.io.File
import java.nio.file.Paths
import java.net.{InetAddress, InetSocketAddress}
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Await

case class Config(bootstrap: Boolean = false,
                  startingPeers: Seq[InetAddress] = Seq(),
                  listeningSocketAddress: InetSocketAddress =
                    new InetSocketAddress(NetworkInterfaces.nonLoopbackInetAddress, 6363),
                  keyFiles: Seq[File] = Seq())

object Raicoin {
  val keyBasename    = "raicoin"
  val privateKeyName = s"$keyBasename.priv"
  val publicKeyName  = s"$keyBasename.pub"

  def main(args: Array[String]): Unit = {
    implicit val config = parseOptions(args)

    val (privateKey, publicKey) = config.keyFiles match {
      case files: Seq[File] => {
        try {
          (FileUtils
             .readFileToByteArray(config.keyFiles(0))
             .asInstanceOf[PrivateKey],
           FileUtils
             .readFileToByteArray(config.keyFiles(1))
             .asInstanceOf[PublicKey])
        } catch {
          case fnfe: java.io.FileNotFoundException => {
            println(fnfe.getMessage)
            sys.exit(1)
          }
        }
      }
      case Nil => {
        println("No private/public key pair provided. Generating")
        val (privKey, pubKey): (PrivateKey, PublicKey) = Curve25519.createKeyPair
        val directory                                  = "."
        val (privateKeyFile, publicKeyFile) =
          (new File(directory, privateKeyName), new File(directory, publicKeyName))
        FileUtils.writeByteArrayToFile(privateKeyFile, privKey)
        FileUtils.writeByteArrayToFile(publicKeyFile, pubKey)
        (privKey, pubKey)
      }
    }

    val system = ActorSystem()
    println("[L]oad existing chain, [R]etrieve from network, [C]reate new chain?")
    val blockchainActorRef = readCharOneOf("lrc") match {
      case 'l' => {
        print("Directory where raicoin.chain is saved: ")
        system.actorOf(
          Props(BlockchainActor.fromSavedBlockchain("raicoin.chain", privateKey, publicKey)))
      }
      case 'r' => {
        system.actorOf(Props(new BlockchainActor(new Blockchain(), privateKey, publicKey)))
      }
      case 'c' => {
        system.actorOf(Props(new BlockchainActor(new Blockchain(), privateKey, publicKey)))
      }
    }

    system.actorOf(Props(new PromptActor(blockchainActorRef, privateKey, publicKey)))
  }

  def parseOptions(args: Array[String]): Config = {
    import scopt.OParser
    val builder = OParser.builder[Config]
    val parser = {
      import builder._
      OParser.sequence(
        programName("raicoin"),
        opt[Unit]("bootstrap")
          .action((x, c) => c.copy(bootstrap = true, startingPeers = Seq())),
        opt[Seq[File]]("key-files")
          .valueName("<private-key-filename>,<public-key-filename>")
          .action((x, c) => c.copy(keyFiles = x)),
      )
    }

    OParser.parse(parser, args, Config()).getOrElse {
      sys.exit(1)
    }
  }

  def readCharOneOf(validChars: String): Char = {
    val keypress = try {
      readChar().toLower
    } catch {
      case eofe: java.io.EOFException => sys.exit(1)
    }
    if (validChars.contains(keypress)) keypress
    else {
      println(s"Invalid choice. Choose one of: [$validChars]")
      readCharOneOf(validChars)
    }
  }

  val asciiArt: String = """

  ooooooooo.              oOo                       oOo
  `OOO   `YOO.            `"'                       `"'
   OOO   .dOO'  .oooo.   oooo   .ooooo.   .ooooo.  oooo  ooo. .oo.
   OOOoooOOP'  `P  )OOb  `OOO  dOO' `"YO dOO' `OOb `OOO  `OOOP"YOOb
   OOO`OOb.     .oP"OOO   OOO  OOO       OOO   OOO  OOO   OOO   OOO
   OOO  `OOb.  dO(  OOO   OOO  OOO   .oO OOO   OOO  OOO   OOO   OOO
  oOOOo  oOOOo `YOOO""Oo oOOOo `YObodOP' `YObodOP' oOOOo oOOOo oOOOo

                                                                  """

}
