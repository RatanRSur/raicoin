package raicoin

import akka.actor._

import scala.io.StdIn._
import scala.concurrent.Await
import scala.concurrent.duration._
import java.io.File

object Raicoin {
  def main(args: Array[String]): Unit = {
    implicit val config = Config.parseOptions(args)

    val blockchainFile = new File(s"./${Config.projectName}.chain")
    val blockchainActorProps = if (blockchainFile.exists()) {
      Props(BlockchainActor.fromSavedBlockchain(blockchainFile))
    } else {
      println(asciiArt)
      Props(new BlockchainActor(new Blockchain()))
    }

    val system = ActorSystem()
    val blockchainActorRef = system.actorOf(blockchainActorProps)
    system.actorOf(Props(new PromptActor(blockchainActorRef)))
    Await.ready(system.whenTerminated, Duration.Inf)
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
