package raicoin

import scala.io.StdIn._
import scorex.crypto._
import scorex.crypto.signatures._
import org.apache.commons.io.FileUtils
import org.apache.commons.io.FilenameUtils
import java.io.File
import java.nio.file.Paths
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Await

object Raicoin {
  val keyBasename = "raicoin"
  val privateKeyName = s"$keyBasename.priv"
  val publicKeyName = s"$keyBasename.pub"

  def main(args: Array[String]): Unit = {
    if (!args.contains("-q")) {
      println(asciiArt)
    }
    println("Public Private Key Pair")
    println("[G]enerate or [P]rovide as file?")
    val keyGenerationNeeded = "Gg".contains(readCharOneOf("GgPp"))
    val (privateKey, publicKey) = if (keyGenerationNeeded) {
      val (privKey, pubKey): (PrivateKey, PublicKey) = Curve25519.createKeyPair
      print("Directory to save raicoin.pub, raicoin.priv: ")
      val directory = readDirectory()
      val (privateKeyFile, publicKeyFile) =
        (new File(directory, privateKeyName),
         new File(directory, publicKeyName))
      FileUtils.writeByteArrayToFile(privateKeyFile, privKey)
      FileUtils.writeByteArrayToFile(publicKeyFile, pubKey)
      loadPrivateAndPublicKey(directory)
    } else {
      print(s"Directory where $privateKeyName and $publicKeyName are saved: ")
      loadPrivateAndPublicKey(readDirectory())
    }

    val system  = ActorSystem()
    println("[L]oad existing chain, [R]etrieve from network?")
    val chainLoadingNeeded = "Ll".contains(readCharOneOf("LlRr"))
    val blockchainActor = if (chainLoadingNeeded) {
      print("Directory where raicoin.chain is saved: ")
      val directory = readDirectory()
      system.actorOf(
        Props(BlockchainActor.fromSavedBlockchain(directory + "raicoin.chain", publicKey)))
    } else {
      system.actorOf(Props(new BlockchainActor(new Blockchain(), publicKey)))
    }

    import scala.concurrent.ExecutionContext.Implicits.global
    while (true) {
      print("> ")
      val command = readLine().trim
      command match {
        case "save" => blockchainActor ! Save(".")
        case "balance" => println(Await.result(blockchainActor.ask(Balance(publicKey))(1.seconds), Duration.Inf))
        case "mining start" => blockchainActor ! StartMining
        case "mining stop" => blockchainActor ! StopMining
        case "" => ()
        case _ => println("Invalid command")
      }
    }
  }

  def readDirectory(): String = {
    val pathSep = Option(System.getProperty("file.separator")).get
    val inputWithPathSep = readLine().stripSuffix(pathSep) + pathSep

    /*expand home dir if needed*/
    if (inputWithPathSep.startsWith("~/")) {
      Option(System.getProperty("user.home")).get + inputWithPathSep.stripPrefix("~")
    } else inputWithPathSep
  }

  def loadPrivateAndPublicKey(directoryName: String): (PrivateKey, PublicKey) = {
    (FileUtils.readFileToByteArray(
       new File(directoryName, privateKeyName)).asInstanceOf[PrivateKey],
     FileUtils.readFileToByteArray(
       new File(directoryName, privateKeyName)).asInstanceOf[PublicKey])
  }

  def readCharOneOf(validChars: String): Char = {
    val keypress = readChar()
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
