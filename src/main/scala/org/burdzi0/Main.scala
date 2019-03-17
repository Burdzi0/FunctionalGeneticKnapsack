package org.burdzi0

import org.burdzi0.individual.PopulationGenerator
import org.burdzi0.knapsack.{Knapsack, KnapsackElement}

object Main {

  def main(args: Array[String]): Unit = {
    val knapsack = new Knapsack(
      new KnapsackElement(12,4),
      new KnapsackElement(10, 6),
      new KnapsackElement(8,5),
      new KnapsackElement(11,7),
      new KnapsackElement(14,3),
      new KnapsackElement(7,1),
      new KnapsackElement(9,6),
      new KnapsackElement(6,4)
    )

    val populationGenerator = new PopulationGenerator(knapsack, 18)

    var population = populationGenerator.generatePopulation(20 * 1000)

    println("Start")

    for(phase <- 1 to 20) {
      println("Phase: " + phase)
      population = population.performComputation(0.4, 0.5)
      println(population.population.length + "\n")
    }

    println(population.getBest())
  }
}
