package raicoin

import akka.io.Tcp
import Serializer._

import scorex.crypto._
import scorex.crypto.signatures._


object TestUtils {

  val (tiamatPrivateKey, tiamatPublicKey): (PrivateKey, PublicKey) = Curve25519.createKeyPair
  val (vecnaPrivateKey, vecnaPublicKey): (PrivateKey, PublicKey) = Curve25519.createKeyPair

  def tcpWritten(obj: Serializable) = {
    Tcp.Write(toByteString(obj))
  }

  def tcpUnwrap[T](supposedlyWrite: Any): T = {
    fromByteString(supposedlyWrite.asInstanceOf[Tcp.Write].data).asInstanceOf[T]
  }
}
