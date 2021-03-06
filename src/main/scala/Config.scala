package raicoin

import scorex.crypto.signatures._
import java.net.{InetAddress, InetSocketAddress}
import java.io.File
import org.apache.commons.codec.binary.Hex
import org.apache.commons.io.FileUtils

case class Config(
    bootstrap: Boolean = false,
    interactive: Boolean = true,
    load: File = Config.chainFile,
    bind: Boolean = true,
    mine: Boolean = false,
    startingPeers: Seq[InetSocketAddress] = Nil,
    listeningSocketAddress: InetSocketAddress =
      new InetSocketAddress(NetworkInterfaces.nonLoopbackInetAddress, Config.defaultPort),
    privateKey: PrivateKey = Config.defaultPrivateKey,
    publicKey: PublicKey = Config.defaultPublicKey)

object Config {
  val projectName    = "raicoin"
  val chainName      = s"$projectName.chain"
  val chainFile      = new File(s"./$chainName")
  val privateKeyName = s"$projectName.priv"
  val publicKeyName  = s"$projectName.pub"
  val defaultPort    = 6363

  lazy val (generatedPrivateKey, generatedPublicKey): (PrivateKey, PublicKey) =
    Curve25519.createKeyPair

  val (defaultPrivateKey, defaultPublicKey) = {
    val directory = "."
    val (privateKeyFile, publicKeyFile) =
      (new File(directory, privateKeyName), new File(directory, publicKeyName))
    val privateKey = if (privateKeyFile.exists()) {
      readKeyFromFile[PrivateKey](privateKeyFile)
    } else {
      FileUtils.writeByteArrayToFile(privateKeyFile, generatedPrivateKey)
      generatedPrivateKey
    }
    val publicKey = if (publicKeyFile.exists()) {
      readKeyFromFile[PublicKey](publicKeyFile)
    } else {
      FileUtils.writeByteArrayToFile(publicKeyFile, generatedPublicKey)
      generatedPublicKey
    }
    (privateKey, publicKey)
  }

  def parseOptions(args: Array[String]): Config = {
    import scopt.OParser
    val builder = OParser.builder[Config]
    val parser = {
      import builder._
      OParser.sequence(
        programName("raicoin"),
        opt[Unit]("bootstrap")
          .action((x, c) => c.copy(bootstrap = true, interactive = false, startingPeers = Nil)),
        opt[Unit]("non-interactive")
          .action((x, c) => c.copy(interactive = false)),
        opt[Unit]("mine")
          .action((x, c) => c.copy(mine = true)),
        opt[String]("private-key")
          .valueName("<hex encoded key or filename>")
          .action((x, c) => c.copy(privateKey = decodeKeyOrReadFromFilename[PrivateKey](x))),
        opt[String]("public-key")
          .valueName("<hex encoded key or filename>")
          .action((x, c) => c.copy(publicKey = decodeKeyOrReadFromFilename[PublicKey](x))),
        opt[File]("load")
          .valueName("<path to saved chain>")
          .action((x, c) => c.copy(load = x)),
        opt[String]("starting-peers")
          .valueName("<hostname> <hostname> ...")
          .action((x, c) => {
            val hostnames = x.split(' ')
            val inetSocketAddresses =
              hostnames.map(hn => new InetSocketAddress(hn, Config.defaultPort))
            c.copy(startingPeers = inetSocketAddresses)
          }),
      )
    }

    OParser.parse(parser, args, Config()).getOrElse {
      sys.exit(1)
    }
  }

  def decodeKeyOrReadFromFilename[T](encodingOrFilename: String): T = {
    try {
      Hex.decodeHex(encodingOrFilename).asInstanceOf[T]
    } catch {
      case de: org.apache.commons.codec.DecoderException =>
        readKeyFromFile[T](new File(encodingOrFilename))
    }
  }

  def readKeyFromFile[T](file: File): T = {
    try {
      FileUtils
        .readFileToByteArray(file)
        .asInstanceOf[T],
    } catch {
      case fnfe: java.io.FileNotFoundException => {
        println(fnfe.getMessage)
        sys.exit(1)
      }
    }
  }
}
