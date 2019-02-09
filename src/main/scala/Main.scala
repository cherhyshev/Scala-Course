import LessonData._

import scala.io.StdIn._
import scala.math.{E, Pi, pow, sqrt}

object Main extends App {

  println(flatten(List(Some(1), None, Some(2), Some(3), None, Some(1), None)))


}

object LessonData {

  def flatten(options: List[Option[Int]]): List[Int] = {
    options.collect {
      case Some(x) => x
    }
  }


  def checkIsSimpleInput(p: (Int, Int)): Either[String, (Int, Int)] = {
    if (p._2 == 0) Left("Zero divisor")
    else if (Math.abs(p._1) > Math.abs(p._2)) Left("Invalid input")
    else Right(p)
  }

  def checkIsSimpleOut(p: (Int, Int)): Either[String, (Int, Int)] = {
    checkIsSimpleInput(p) match {
      case Left("Invalid input") => Left("Improper result")
      case Left("Zero divisor") => Left("Zero divisor")
      case _ => Right(simplify(p._1, p._2))
    }
  }


  def simplify(p: (Int, Int)): (Int, Int) = {
    val gcdP = BigInt(p._2) gcd p._1
    (p._1 / gcdP.toInt, p._2 / gcdP.toInt)
  }


  def divide(p: (Int, Int))(q: (Int, Int)): Either[String, (Int, Int)] = {
    checkIsSimpleInput(p).flatMap { pe =>
      checkIsSimpleInput(q).flatMap { qe =>
        checkIsSimpleOut(pe._1 * qe._2, qe._1 * pe._2)
      }
    }
  }


  def foo(list: List[Int]): Int = {
    val res = list.find(_ % 3 == 0)
    res match {
      case Some(value) => value * 2
      case None => 0
    }
  }


  def swap3(tuple: (Int, Int, Int)): (Int, Int, Int) = (tuple._3, tuple._2, tuple._1)


  case class Jar(name: String, value: Int, price: Double)

  def discount: PartialFunction[Jar, String] = {
    case jar: Jar if jar.value > 5 && jar.value <= 10 => jar.name + " " + jar.price * 0.05
    case jar: Jar if jar.value > 10 => jar.name + " " + jar.price * 0.10
  }

  val log: PartialFunction[Double, Double] = {
    case num if num > 0 || num.isNaN => Math.log(num)
  }


  def searchInArray(cond: Int => Boolean, array: List[Int]): List[Int] = {
    array.filter(cond)
  }

  def middle[T](data: Iterable[T]): T = {
    data.splitAt(data.size / 2)._2.head
  }

  import scala.annotation.tailrec

  @tailrec
  def fibs(n: Int, currentNumber: Int = 1, f1: BigInt = 0, f2: BigInt = 1): BigInt = {
    if (n == currentNumber)
      f2
    else {
      fibs(n, currentNumber + 1, f2, f1 + f2)
    }
  }


  def isCapital(word: String, pos: Int): Boolean = {
    word.charAt(pos).isUpper
  }


  def normalDistribution(mu: Double, sigma: Double, x: Double): Double = {
    val res = 1 / (sigma * sqrt(Pi * 2)) * pow(E, -pow(x - mu, 2) / 2 * pow(sigma, 2))
    res
  }

  import scala.math.BigDecimal.RoundingMode.HALF_UP

  def crispsWeight(weight: BigDecimal, potatoWaterRatio: Double, crispsWaterRatio: Double): BigDecimal = {
    val res = weight * (1 - potatoWaterRatio) / (1 - crispsWaterRatio)
    res.setScale(5, HALF_UP)
  }

  case class Pet(name: String, says: String)

}
