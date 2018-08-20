package raicoin

import akka.actor._
import akka.testkit.TestProbe
import org.scalatest._
import java.net.InetAddress
import scala.concurrent.duration._
import BlockchainActor._
import Serializer._
import akka.io.Tcp

class ActorNetworking extends FunSuiteLike with TestChains {

  test("remote actor automatically finds local actor and updates itself") {

    //bind to 6364
    val systemA = ActorSystem("A")
    val actorA  = systemA.actorOf(Props(new BlockchainActor(length4chain, None)), "A")

    //bind to 6363
    val systemB = ActorSystem("B")
    val actorB  = systemB.actorOf(Props(new BlockchainActor(rootOnly)), "B")

    val p                      = TestProbe("p")(systemB)
    implicit val defaultSender = p.testActor

    Thread.sleep(1000)
    actorB ! Request(3)
    p.expectMsg(1.seconds, Tcp.Write(serialize(length4chain(3))))
    systemB.terminate()
    systemA.terminate()
  }

}
