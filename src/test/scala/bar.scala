import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}


object Bar {
  def apply(nbServers: Int): Props = Props(new Bar(nbServers))
}

class Bar(nbServers: Int) extends Actor with ActorLogging {

  var serveurs: Vector[ActorRef] = Vector.empty
  var barman: ActorRef = _
  var currentServerIndex = 0

  import ActorClient._

  override def preStart(): Unit = {

    barman = context.actorOf(ActorBarMan(), "barMan")

    serveurs = (1 to nbServers).map(x => context.actorOf(ActorServer(barman), s"serveur$x")).to(Vector)
  }


  override def receive: Receive = {
    case "Bonjour" =>
      currentServerIndex += 1
      currentServerIndex = currentServerIndex % nbServers
      sender ! VotreServer(serveurs(currentServerIndex))
    case msg@_ => log.info(s"j'ai reçu le message : $msg")
  }

}


// Client
object ActorClient {

  // commands of client
  case class VotreServer(server: ActorRef)

  case class ResponseServer(msg: String = "oui monsieur")

  case class LivraisonToClient(commande: String, prix: Double)

  def apply(bar: ActorRef): Props = Props(new ActorClient(bar))
}

class ActorClient(bar: ActorRef) extends Actor with ActorLogging {

  import ActorServer._
  import ActorClient._


  override def preStart(): Unit = {
    log.info(s"client ${self.path.name} rentre au bar")
    commander(bar)
  }

  def commander(bar: ActorRef): Unit = {
    log.info("client commande")
    bar ! "Bonjour"
  }

  def receive: Receive = {
    case vs: VotreServer =>
      log.info(s"${vs.server.path.name}: je suis associé à ${self.path.name}")
      vs.server ! Commande("une commande")
    case lvrC: LivraisonToClient =>
      log.info(s"j'ai reçu ma commande : ${lvrC.commande} qui coute ${lvrC.prix}")
      sender ! Paiement(lvrC.prix)
    case "Bonne Appetit" => sender ! "Merci"
    case msg@_ => log.info(s"j'ai reçu le message : $msg")
  }
}


// Server
object ActorServer {

  // commands of server
  case class Commande(commande: String)

  case class Livraison(demandeur: ActorRef, commande: String, prix: Double)

  case class Paiement(prix: Double)


  def apply(barMan: ActorRef): Props = Props(new ActorServer(barMan))
}

class ActorServer(barMan: ActorRef) extends Actor with ActorLogging {

  import ActorClient._
  import ActorBarMan._
  import ActorServer._


  def receive: Receive = {
    case cmd: Commande =>
      log.info(s"${self.path.name}: j'ai reçu la commande de ${sender.path.name}")
      barMan ! CommandeClient(sender, cmd.commande)
    case lvr: Livraison =>
      log.info(s"${self.path.name}: j'ai reçu la livraison de ${sender.path.name}")
      lvr.demandeur ! LivraisonToClient(lvr.commande, lvr.prix)
    case pmt: Paiement =>
      log.info(s"${self.path.name}: j'ai reçu le paiement de ${pmt.prix} de ${sender.path.name}")
      // Encaissement
      sender ! "Bonne Appetit"
    case msg@_ => log.info(s"j'ai reçu le message : $msg")
  }
}


// Bar man
object ActorBarMan {

  case class CommandeClient(demandeur: ActorRef, commande: String)


  def apply(): Props = Props(new ActorBarMan())
}

class ActorBarMan() extends Actor with ActorLogging {

  import ActorBarMan._
  import ActorServer._

  override def preStart(): Unit = {
    log.info(s"barMan start")
  }

  def receive: Receive = {
    case cmdClient: CommandeClient =>
      //      préparer la commande du client et calculer le prix
      val prix =  math.ceil(100 + (Math.random() * (1000 - 100)))
      sender ! Livraison(cmdClient.demandeur, cmdClient.commande, prix)
    case msg@_ => log.info(s"j'ai reçu le message : $msg")
  }
}


object mainBar extends App {
  println("Hello")

  val systeme = ActorSystem("act_sys")
  val bar = systeme.actorOf(Bar(3), "Bar_d_avenir")

  val clients = (1 to 10).map(x => {
    systeme.actorOf(ActorClient(bar), s"client$x")
  })

}
