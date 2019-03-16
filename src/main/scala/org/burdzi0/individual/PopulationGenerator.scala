package org.burdzi0.individual

import org.burdzi0.knapsack.Knapsack

import scala.annotation.tailrec
import scala.util.Random

class PopulationGenerator(val knapsack: Knapsack, fitness:Int) {

  private val rnd = new Random()

  def generatePopulation(size:Int): Population = {
    println("Create population")
    new Population(createNewIndividuals(size), fitness)
  }

  def createNewIndividuals(number:Int) : List[Individual] = {

    @tailrec
    def createIndTailRec(size:Int, list: List[Individual]): List[Individual] = {
      size match {
        case 0 => list
        case s if s > 0 => createIndTailRec(size - 1, createNewIndividual()::list)
      }
    }

    createIndTailRec(number, Nil)
  }

  def createNewIndividual(): Individual = {
    def generateRandomKnapsackContent(): List[Boolean] = {
      knapsack.elements.map(_ => rnd.nextBoolean())
    }

    def createValidIndividual(): Individual = {
      var ind = new Individual(generateRandomKnapsackContent(), knapsack)
      while (ind.calculateFitness() > fitness) {
        ind = createValidIndividual()
      }
      ind
    }

    createValidIndividual()
  }

}
