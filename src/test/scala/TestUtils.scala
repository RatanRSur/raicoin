package raicoin

import akka.io.Tcp
import Serializer._

object TestUtils {
  def tcpWritten(obj: Serializable) = {
    Tcp.Write(serialize(obj))
  }
}
