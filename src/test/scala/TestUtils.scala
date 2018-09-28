package raicoin

import akka.io.Tcp
import Serializer._

object TestUtils {
  def tcpWritten(obj: Serializable) = {
    Tcp.Write(serialize(obj))
  }

  def retriesOnTimeout[T](n: Int)(block: =>T): T = {
    require(n >= 0)
    try {
      block
    } catch {
      case ae: AssertionError => if (ae.getMessage.contains("timeout") && n > 0) {
        retriesOnTimeout(n-1)(block)
      } else throw ae
    }
  }
}
