package genetic

import scala.collection.mutable

case class Organism(chromosome: mutable.Buffer[Move])

object Organism {

  def create(length: Int): Organism =
    Organism(mutable.Buffer.fill(length)(Move.create))
}
