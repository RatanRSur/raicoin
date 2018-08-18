package raicoin

import akka.util.ByteString

object Serializer {
  def serialize(x: Serializable): ByteString =
    ByteString(org.apache.commons.lang3.SerializationUtils.serialize(x))

  def deserialize(x: ByteString): Any =
    org.apache.commons.lang3.SerializationUtils.deserialize(x.toArray)

}
