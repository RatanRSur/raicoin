package raicoin

import scala.collection.immutable.SortedMap
import scala.util.Try
import akka.util.ByteString
import spray.json._
import spray.json.DefaultJsonProtocol._
import scorex.crypto.signatures._
import java.net.InetSocketAddress

object Serializer {

  def toByteString[T](x: T)(implicit format: JsonFormat[T]): ByteString =
    ByteString(serializeWithName(x))
  def fromByteString(x: ByteString): AnyRef = deserializeWithName(x.toArray)

  def serialize[T](x: T)(implicit format: JsonFormat[T]): Array[Byte] =
    x.toJson.prettyPrint.getBytes
  def serializeWithName[T](x: T)(implicit format: JsonFormat[T]): Array[Byte] = {
    val EOI = '\uFFFF'
    val jsonWithName = JsObject(
      Map("name" -> JsString(x.getClass.getSimpleName.stripSuffix("$")), "message" -> x.toJson))
      .prettyPrint
    (jsonWithName + EOI).getBytes
  }
  def deserialize[T](x: Array[Byte])(implicit format: JsonFormat[T]): T =
    (new String(x)).parseJson.convertTo[T]
  def deserializeWithName(x: Array[Byte]): AnyRef = {
    val json = (new String(x)).parseJson
    RaicoinJsonProtocols.read(json)
  }

  object RaicoinJsonProtocols extends DefaultJsonProtocol {
    implicit val SortedMapProtocol = new RootJsonFormat[SortedMap[String, Long]] {
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
    implicit val LedgerProtocol    = jsonFormat1(Ledger)
    implicit val RootBlockProtocol = jsonFormat1(RootBlock)
    implicit val ByteStringProtocol = new RootJsonFormat[ByteString] {
      def write(bs: ByteString) = DefaultJsonProtocol.arrayFormat[Byte].write(bs.toArray)
      def read(value: JsValue)  = ByteString(DefaultJsonProtocol.arrayFormat[Byte].read(value))
    }
    implicit val PublicKeyProtocol = new RootJsonFormat[PublicKey] {
      def write(pk: PublicKey) = DefaultJsonProtocol.arrayFormat[Byte].write(pk)
      def read(value: JsValue) =
        DefaultJsonProtocol.arrayFormat[Byte].read(value).asInstanceOf[PublicKey]
    }
    implicit val TransactionProtocol       = jsonFormat4(Transaction)
    implicit val SignedTransactionProtocol = jsonFormat2(SignedTransaction)
    implicit val MinedBlockProtocol        = jsonFormat6(MinedBlock)
    implicit val BlockJsonFormat = new RootJsonFormat[Block] {
      def write(p: Block) = p match {
        case rb: RootBlock  => rb.toJson
        case mb: MinedBlock => mb.toJson
      }

      def read(value: JsValue) = value match {
        case obj: JsObject if (obj.fields.size == 6) => value.convertTo[MinedBlock]
        case obj: JsObject                           => value.convertTo[RootBlock]
      }
    }
    implicit val BlockchainProtocol         = jsonFormat3(Blockchain.apply)
    implicit val RequestBlocksSinceProtocol = jsonFormat1(RequestBlocksSince)
    implicit val InetSocketAddressProtocol = new RootJsonFormat[InetSocketAddress] {
      def write(insa: InetSocketAddress) =
        JsArray(JsString(insa.getHostName), JsNumber(insa.getPort))

      def read(value: JsValue) = value match {
        case JsArray(Vector(JsString(hostname), JsNumber(port))) =>
          new InetSocketAddress(hostname, port.toInt)
        case _ => deserializationError("InetSocketAddress expected")
      }
    }
    implicit val GetPeersProtocol = new RootJsonFormat[GetPeers.type] {
      def write(gp: GetPeers.type) = JsObject()
      def read(value: JsValue)     = GetPeers
    }
    def read(value: JsValue): AnyRef = {
      val jsObj = value.asJsObject
      val className =
        jsObj
          .fields("name")
          .asInstanceOf[JsString]
          .value
      val protocol = className match {
        case "MinedBlock"         => MinedBlockProtocol
        case "RequestBlocksSince" => RequestBlocksSinceProtocol
        case "SortedMap"          => SortedMapProtocol
        case "Ledger"             => LedgerProtocol
        case "ByteString"         => ByteStringProtocol
        case "PublicKey"          => PublicKeyProtocol
        case "Transaction"        => TransactionProtocol
        case "SignedTransaction"  => SignedTransactionProtocol
        case "RootBlock"          => BlockJsonFormat
        case "Blockchain"         => BlockchainProtocol
        case "InetSocketAddress"  => InetSocketAddressProtocol
        case "GetPeers"           => GetPeersProtocol
      }
      protocol.read(jsObj.fields("message"))
    }
  }
}
