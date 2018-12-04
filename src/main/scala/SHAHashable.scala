package raicoin

import java.security.MessageDigest
import java.nio.ByteBuffer
import scala.collection.SortedSet
import scala.collection.immutable.ListMap

import scorex.crypto.signatures._

object HashImplicits extends Serializable {

  // TODO: martin odersky says to make the implicits as specific as possible
  // I think he means that I should merge these and use the type condition set in hashable seq
  // that the element type should be hashable
  // I ran into problems doing this for HashableSet because it's element type (String)
  // would itself need to be implicitly converted to HashableString so the compiler complained
  // about string not being hashable
  implicit class HashableSeq[A <: SHAHashable](collection: Seq[A]) extends SHAHashable with Serializable {
    val hash: Array[Byte] = {
      val sha = MessageDigest.getInstance("SHA-256")
      collection.map(_.hash).foreach(sha.update)
      sha.digest
    }
  }

  implicit class HashableSet(collection: SortedSet[String]) extends SHAHashable with Serializable {
    val hash: Array[Byte] = {
      val sha = MessageDigest.getInstance("SHA-256")
      collection.map(_.hash).foreach(sha.update)
      sha.digest
    }
  }

  implicit class HashableListMap(collection: ListMap[String, Long]) extends SHAHashable with Serializable {
    val hash: Array[Byte] = {
      val sha = MessageDigest.getInstance("SHA-256")
      collection.flatMap { case (k, v) => Seq(k.hash, v.hash) }.foreach(sha.update)
      sha.digest
    }
  }

  implicit class HashableLong(long: Long) extends SHAHashable with Serializable {
    val hash: Array[Byte] = {
      val sha = MessageDigest.getInstance("SHA-256")
      val buf = ByteBuffer.allocate(8).putLong(long)
      sha.update(buf)
      sha.digest
    }
  }

  implicit class HashableString(string: String) extends SHAHashable with Serializable {
    val hash: Array[Byte] = {
      val sha = MessageDigest.getInstance("SHA-256")
      sha.update(string.getBytes("UTF-8"))
      sha.digest
    }
  }
}

trait SHAHashable {
  import HashImplicits._
  val hash: Array[Byte]
}
