package raicoin

import java.io.File
import org.apache.commons.io.FileUtils
import TestUtils._

import Serializer._
import Serializer.RaicoinJsonProtocols._

object ResourceGeneration {

  def main(args: Array[String]) = {
    val testTransactionsWithMiners =
      testTransactions.zip(Seq(vecnaPublicKey, tiamatPublicKey, tiamatPublicKey))

    val testChains = Seq
      .iterate((0, new Blockchain()), 4) {
        case (i, chain) =>
          val (testTransaction, miner) = testTransactionsWithMiners(i)
          (i + 1, chain.mineBlock(testTransaction, miner))
      }
      .map(_._2)

    val resourceDirectoryName = "../src/test/scala/resources"
    new File(resourceDirectoryName).mkdirs()
    testChains.map(x => serialize(x)).zipWithIndex.foreach {
      case (bytes: Array[Byte], i) =>
        FileUtils.writeByteArrayToFile(new File(s"$resourceDirectoryName/length${i + 1}.chain"),
                                       bytes)
    }
  }
}
