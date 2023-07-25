package calculator

import scala.collection.mutable

case class Env(val e: mutable.Map[String, Value])