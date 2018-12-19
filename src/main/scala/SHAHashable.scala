package raicoin

import java.nio.ByteBuffer
import java.security.MessageDigest

import scala.collection.SortedSet
import scala.collection.immutable.ListMap

object HashImplicits extends Serializable {

  implicit class HashableSeq[A <: SHAHashable](collection: Seq[A])
      extends SHAHashable
      with Serializable {
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

  implicit class HashableListMap(collection: ListMap[String, Long])
      extends SHAHashable
      with Serializable {
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
  val hash: Array[Byte]
}
