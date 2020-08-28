package genetic

import scala.collection.mutable
import scala.util.Random

class Population(populationSize: Int, solutionSize: Int) {
  val population: mutable.Buffer[Organism] = mutable.Buffer.empty[Organism]

  val mutationRate = 0.015
  val mixingRatio  = 0.5

  val tournamentPopulation: mutable.Buffer[Organism] = mutable.Buffer.empty[Organism]
  val tournamentSize = 5

  for (_ <- 0 to tournamentSize) {
    tournamentPopulation.addOne(Organism.create(solutionSize))
  }


  /**
    * Populate with organisms
    */
  def populate(): Unit = {
    val organisms = for {
      _     <- 0 until populationSize
    } yield Organism.create(solutionSize)

    population.addAll(organisms)
  }

  /**
    * Evolve the population by crossover and mutation
    * @param elitist If true, the fittest organism passes to the next generation
    * @param evaluator The evaluator to use
    */
  def evolve(elitist: Boolean, evaluator: Evaluator): Unit = {
    val eliteOrganism = evaluator.fittest(this.population)
    val newGeneration = for {
      _       <- 1 until populationSize
      parent1 = select(evaluator)
      parent2 = select(evaluator)
      child   = crossover(parent1, parent2)
      _ = mutate(child)
    } yield child

    population.clear()
    population.addAll(newGeneration)
    population.insert(0, eliteOrganism)
  }

  /**
    * Mutate an organism with a random rate of 0.015
    */
  def mutate(organism: Organism): Unit =
      organism.chromosome.update(Random.nextInt(solutionSize), Move.create)

  /**
    * Create a child organism from two parents
    */
  def crossover(parent1: Organism, parent2: Organism): Organism = {
    val childChromosome: mutable.Buffer[Move] =
      parent1.chromosome.zip(parent2.chromosome).map { case (p1, p2) => if (Math.random <= mixingRatio) p1 else p2 }

    Organism(childChromosome)
  }

  /**
    * Select an organism from the population using stochastic universal sampling
    */
  def select(evaluator: Evaluator): Organism = {

    for (i <- 0 to tournamentSize) {
      val randomOrganism = population(Random.nextInt(tournamentSize))
      tournamentPopulation.update(i, randomOrganism)
    }

    evaluator.fittest(tournamentPopulation)
  }
}
