package genetic

class Evaluator(solution: String) {
  val solutionBytes: IndexedSeq[Byte] = solution.map(_.asDigit.toByte)

  /**
    * Return the fittest organism in a population
    */
  def fittest(population: Population): Organism = population.population.maxBy(fitness)

  /**
    * Calculate an organism's fitness by comparing it to the optimal solution
    */
  def fitness(organism: Organism): Double = {
    val score = organism.chromosome.zip(solutionBytes).count { case (a, b) => a == b }

    1.0 - ((organism.chromosome.length - score) / 100.0)
  }
}
