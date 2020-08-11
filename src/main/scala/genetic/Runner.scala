package genetic

trait Evolver {

  def run(evaluator: Evaluator, population: Population): Organism = {

    @scala.annotation.tailrec
    def run(pop: Population, generation: Int): Organism = {
      val fittest = evaluator.fittest(pop)
      val fitness = evaluator.fitness(fittest)

      println(f"generation: $generation%02d chromosome: $fittest%s fitness: $fitness%2.2f")

      if (fitness >= 1.0)
        fittest
      else
        run(
          pop.evolve(elitist = true, evaluator),
          generation + 1
        )
    }

    run(population, 1)
  }
}

object Runner extends App with Evolver {
  val candidate = "01" * 32
  val evaluator = new Evaluator(candidate)

  val population = new Population(50)
  population.populate()

  val solution: Organism = run(evaluator, population)

  println("\ncandidate:  " + candidate)
  println("solution:   " + solution)
}
