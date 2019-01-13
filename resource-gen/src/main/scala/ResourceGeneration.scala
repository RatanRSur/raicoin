package raicoin

import java.io.File
import org.apache.commons.io.FileUtils
import org.apache.commons.lang3.SerializationUtils.serialize
import TestUtils._

object ResourceGeneration extends App {
  val testTransactionsWithMiners =
    testTransactions.zip(Seq(vecnaPublicKey, tiamatPublicKey, tiamatPublicKey))

  val resourceDirectoryName = "src/test/scala/resources"
  new File(resourceDirectoryName).mkdirs()
  testChains.map(x => serialize(x)).zipWithIndex.foreach {
    case (bytes: Array[Byte], i) =>
      FileUtils.writeByteArrayToFile(new File(s"$resourceDirectoryName/length$i.chain"), bytes)
  }
}
