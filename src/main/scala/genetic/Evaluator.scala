package genetic

class Evaluator(solution: IndexedSeq[Move]) {

  def solutionLength: Int = solution.length

  /**
   * Return the fittest organism in a population
   */

  def fittest(population: Population): Organism = population.population.maxBy(fitness)

  /**
    * Calculate an organism's fitness by comparing it to the optimal solution
    */
  def fitness(organism: Organism): Double = {
    val score = organism.chromosome.zip(solution).count { case (a, b) => a == b }

    1.0 - ((solution.length - score) / solution.length.toDouble)
  }
}
