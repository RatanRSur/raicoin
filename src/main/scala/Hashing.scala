import java.security.MessageDigest
import java.nio.ByteBuffer
import scala.collection.SortedSet
import scala.collection.immutable.ListMap

trait SHAHashable {
  private val sha = MessageDigest.getInstance("SHA-256")

  val hashDependencies: Seq[Array[Byte]]
  lazy val hash: Array[Byte] = {
    hashDependencies.foreach(sha.update)
    sha.digest
  }

  // TODO: martin odersky says to make the implicits as specific as possible
  // I think he means that I should merge these and use the type condition set in hashable seq
  // that the element type should be hashable
  // I ran into problems doing this for HashableSet because it's element type (String)
  // would itself need to be implicitly converted to HashableString so the compiler complained
  // about string not being hashable
  implicit class HashableSeq[A <: SHAHashable](collection: Seq[A]) extends SHAHashable {
    val hashDependencies = collection.map(_.hash)
  }

  implicit class HashableSet(collection: SortedSet[String]) extends SHAHashable {
    val hashDependencies = collection.map(_.hash).toSeq
  }

  implicit class HashableListMap(collection: ListMap[String, Long]) extends SHAHashable {
    val hashDependencies = collection.flatMap { case (k, v) => Seq(k.hash, v.hash) }.toSeq
  }

  implicit class HashableLong(long: Long) extends SHAHashable {
    val hashDependencies = {
      val buf = ByteBuffer.allocate(8).putLong(long)
      Seq(buf.array)
    }
  }

  implicit class HashableString(string: String) extends SHAHashable {
    val hashDependencies = Seq(string.getBytes("UTF-8"))
  }
}
