package genetic

import scala.collection.mutable
import scala.util.Random

class Population(populationSize: Int, solutionSize: Int) {
  val population: mutable.Buffer[Organism] = mutable.Buffer.empty[Organism]

  val mutationRate = 0.015
  val mixingRatio  = 0.5

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
    * Return the population size
    */
  def size: Integer = population.length

  /**
    * Add an organism to a particular location in a population
    */
  def addOrganism(index: Integer, organism: Organism): Unit = population.insert(index, organism)

  /**
    * Evolve the population by crossover and mutation
    * @param elitist If true, the fittest organism passes to the next generation
    * @param evaluator The evaluator to use
    */
  def evolve(elitist: Boolean, evaluator: Evaluator): Unit = {
    val eliteOrganism = evaluator.fittest(this)
    for {
      index       <- 1 until populationSize
      parent1 = select(evaluator)
      parent2 = select(evaluator)
      child   = crossover(parent1, parent2)
      _ = mutate(child)
      _ = population.insert(index, child)
    } yield ()

    population.insert(0, eliteOrganism)
  }

  /**
    * Mutate an organism with a random rate of 0.015
    */
  def mutate(organism: Organism): Unit =
      organism.chromosome.insert(Random.nextInt(solutionSize), Move.create)

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
    val numberOfRounds = 5

    val tournament = new Population(numberOfRounds, evaluator.solutionLength)

    for (i <- 0 to numberOfRounds) {
      val randomOrganism = population(Random.nextInt(populationSize))
      tournament.addOrganism(i, randomOrganism)
    }

    evaluator.fittest(tournament)
  }
}
