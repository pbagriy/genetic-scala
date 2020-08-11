package genetic

case class Organism(chromosome: IndexedSeq[Byte]) {
  override def toString: String = chromosome.mkString
}
