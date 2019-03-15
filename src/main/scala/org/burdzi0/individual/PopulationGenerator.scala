package org.burdzi0.individual

import org.burdzi0.knapsack.Knapsack

import scala.util.Random

class PopulationGenerator(knapsack: Knapsack) {

  def generatePopulation(size:Int): Population = {
    new Population(createNewIndividuals(size))
  }

  def createNewIndividuals(number:Int) : List[Individual] = {
    number match {
      case 0 => Nil
      case _ => createNewIndividual()::createNewIndividuals(number - 1)
    }
  }

  def createNewIndividual(): Individual = {
    new Individual(knapsack.elements.map(_ => new Random().nextBoolean()))
  }

}
