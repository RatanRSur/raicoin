package raicoin

import akka.util.ByteString
import spray.json._
import spray.json.DefaultJsonProtocol._
import scorex.crypto.signatures._
import org.apache.commons.lang3.SerializationUtils.{
  serialize => javaSerialize,
  deserialize => javaDeserialize
}

object Serializer {

  //def toByteString[T](x: T)(implicit writer: JsonFormat[T]): ByteString = ByteString(serialize[T](x))
  //def fromByteString[T](x: ByteString)(implicit reader: JsonFormat[T]): Any = deserialize[T](x.toArray)
  def toByteString(x: Serializable): ByteString = ByteString(javaSerialize(x))

  def fromByteString(x: ByteString): Any = javaDeserialize(x.toArray)

  def serialize[T](x: T)(implicit writer: JsonFormat[T]): Array[Byte] =
    x.toJson.prettyPrint.getBytes
  def deserialize[T](x: Array[Byte])(implicit reader: JsonFormat[T]): T =
    (new String(x)).parseJson.convertTo[T]

  object RaicoinJsonProtocols extends DefaultJsonProtocol {
    implicit val ledgerProtocol    = jsonFormat1(Ledger)
    implicit val rootBlockProtocol = jsonFormat1(RootBlock)
    implicit val byteStringProtocol = new RootJsonFormat[ByteString] {
      def write(bs: ByteString) = DefaultJsonProtocol.arrayFormat[Byte].write(bs.toArray)
      def read(value: JsValue)  = ByteString(DefaultJsonProtocol.arrayFormat[Byte].read(value))
    }
    implicit val publicKeyProtocol = new RootJsonFormat[PublicKey] {
      def write(pk: PublicKey) = DefaultJsonProtocol.arrayFormat[Byte].write(pk)
      def read(value: JsValue) =
        DefaultJsonProtocol.arrayFormat[Byte].read(value).asInstanceOf[PublicKey]
    }
    implicit val transactionBlockProtocol       = jsonFormat3(Transaction)
    implicit val signedTransactionBlockProtocol = jsonFormat2(SignedTransaction)
    implicit val minedBlockProtocol             = jsonFormat6(MinedBlock)
    implicit object BlockJsonFormat extends RootJsonFormat[Block] {
      def write(p: Block) = p match {
        case rb: RootBlock  => rb.toJson
        case mb: MinedBlock => mb.toJson
      }

      def read(value: JsValue) = value match {
        case obj: JsObject if (obj.fields.size == 6) => value.convertTo[MinedBlock]
        case obj: JsObject                           => value.convertTo[RootBlock]
      }
    }
    implicit val blockchainProtocol         = jsonFormat3(Blockchain.apply)
    implicit val requestBlocksSinceProtocol = jsonFormat1(RequestBlocksSince)
  }
}
