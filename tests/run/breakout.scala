import scala.collection.generic._
import scala.collection._
import scala.collection.mutable._

object Test extends dotty.runtime.LegacyApp {
	val l = List(1, 2, 3)
	val a: Array[Int] =  l.map(_ + 1)(breakOut)
	println(a.mkString(", "))
}
