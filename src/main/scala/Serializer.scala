package raicoin

import scala.collection.immutable.SortedMap
import scala.util.Try
import akka.util.ByteString
import spray.json._
import spray.json.DefaultJsonProtocol._
import scorex.crypto.signatures._
import java.net.InetSocketAddress

object Serializer {

  def toByteString[T](x: T)(implicit format: JsonFormat[T]): ByteString = ByteString(serialize(x))
  def fromByteString(x: ByteString): AnyRef                             = typelessDeserialize(x.toArray)

  def serialize[T](x: T)(implicit format: JsonFormat[T]): Array[Byte] = {
    val EOI = '\uFFFF'
    (x.toJson.compactPrint + EOI).getBytes
  }
  def deserialize[T](x: Array[Byte])(implicit format: JsonFormat[T]): T =
    (new String(x)).parseJson.convertTo[T]
  def typelessDeserialize(x: Array[Byte]): AnyRef = {
    val json = (new String(x)).parseJson
    import RaicoinJsonProtocols._
    Stream(
      minedBlockProtocol,
      requestBlocksSinceProtocol,
      sortedMapProtocol,
      ledgerProtocol,
      rootBlockProtocol,
      byteStringProtocol,
      publicKeyProtocol,
      transactionBlockProtocol,
      signedTransactionBlockProtocol,
      blockJsonFormat,
      blockchainProtocol,
      inetSocketAddressProtocol
    ).flatMap(format => Try(format.read(json)).toOption).head
  }

  object RaicoinJsonProtocols extends DefaultJsonProtocol {
    implicit val sortedMapProtocol = new RootJsonFormat[SortedMap[String, Long]] {
      def write(m: SortedMap[String, Long]) = JsObject {
        m.map { field =>
          field._1.toJson match {
            case JsString(x) => x -> field._2.toJson
            case x =>
              throw new SerializationException(
                "Map key must be formatted as JsString, not '" + x + "'")
          }
        }
      }
      def read(value: JsValue) =
        SortedMap.empty[String, Long](Ordering.String) ++ DefaultJsonProtocol
          .mapFormat[String, Long]
          .read(value)
    }
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
    implicit val transactionBlockProtocol       = jsonFormat4(Transaction)
    implicit val signedTransactionBlockProtocol = jsonFormat2(SignedTransaction)
    implicit val minedBlockProtocol             = jsonFormat6(MinedBlock)
    implicit val blockJsonFormat = new RootJsonFormat[Block] {
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
    implicit val inetSocketAddressProtocol = new RootJsonFormat[InetSocketAddress] {
      def write(insa: InetSocketAddress) =
        JsArray(JsString(insa.getHostName), JsNumber(insa.getPort))

      def read(value: JsValue) = value match {
        case JsArray(Vector(JsString(hostname), JsNumber(port))) =>
          new InetSocketAddress(hostname, port.toInt)
        case _ => deserializationError("InetSocketAddress expected")
      }
    }
  }
}
