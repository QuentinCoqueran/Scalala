package example

import scala.io.StdIn

import com.typesafe.config.{Config, ConfigFactory}

// Lancer l'exemple avec: sbt "runMain example.Hello"
// TODO: a supprimer
object Hello extends Greeting with App {
  println(":> Enter your name: ")
  val user = StdIn.readLine()
  println(s":> $greeting $user")

  val conf: Config = ConfigFactory.load()
  val message: String = conf.getString("example.message")
  println(s":> message: $message")
  val one: Int = conf.getInt("example.one")
  println(s":> one: ${one.toString}")
  val yes: Boolean = conf.getBoolean("example.yes")
  println(s":> yes: ${yes.toString}")
}

trait Greeting {
  lazy val greeting: String = "hello"
}
