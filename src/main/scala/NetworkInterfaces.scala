package raicoin

import java.net.DatagramSocket
import java.net.{InetAddress, InetSocketAddress}

object NetworkInterfaces {

  def nonLoopbackInetAddress(): InetAddress = {
    val socket = new DatagramSocket()
    socket.connect(InetAddress.getByName("8.8.8.8"), 10002)
    socket.getLocalAddress
  }
}
