package genetic

import scala.util.Random

case class Move(thrust: Int, angle: Int)

object Move {
  def create: Move = Move(Random.between(0, 5),Random.between(-15, 16))
}