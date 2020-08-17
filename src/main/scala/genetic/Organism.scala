package genetic

import scala.util.Random

case class Organism(chromosome: String)

object Organism {
  private val lowerCase = ('a' to 'z').mkString
  private val upperCase = ('A' to 'Z').mkString
  private val numbers = (0 to 9).mkString
  private val symbols = "!? ,.-'"
  private val alphabet = lowerCase + upperCase + numbers + symbols

  def create(length: Int): Organism = {
    val chromosome = Seq.fill(length)(alphabet(Random.nextInt(alphabet.length))).mkString
    Organism(chromosome)
  }

  def getRandomChar: Char = alphabet(Random.nextInt(alphabet.length))
}
