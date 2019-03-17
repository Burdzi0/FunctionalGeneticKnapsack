package org.burdzi0.individual

import scala.annotation.tailrec
import scala.util.Random
import scala.math.abs

class Population(val population: List[Individual], fitness:Int) {

  val rnd = new Random(System.currentTimeMillis())

  def cross(size:Int):List[Individual] = {

    @tailrec
    def crossTailRec(size:Int, list: List[List[Individual]]) : List[Individual] = {
      size match {
        case 0 => list.flatten
        case s if s > 0 => crossTailRec(s - 2, chooseRandomIndividual().cross(chooseRandomIndividual(), fitness)::list)
      }
    }

    crossTailRec(size, Nil)
  }

  def mutate(size:Int) : List[Individual] = {

    @tailrec
    def mutateTailRec(size:Int, list: List[Individual]): List[Individual] = {
      size match {
        case 0 => list
        case s if s > 0 => mutateTailRec(size - 1, chooseRandomIndividual().mutate(fitness)::list)
      }
    }

    mutateTailRec(size, Nil)
  }

  private def fillWithRandom(size:Int) : List[Individual] = {

    @tailrec
    def fillWithRandomTail(size:Int, list: List[Individual]): List[Individual] = {
      (size, list) match {
        case (0, _) => list
        case (s, l) if s > 0 => fillWithRandomTail(s - 1, chooseRandomIndividual()::l)
      }
    }

    fillWithRandomTail(size, Nil)
  }

  def chooseRandomIndividual(): Individual = {
    population(abs(rnd.nextInt(population.length)))
  }

  def performComputation(crossPopulationPercent:Double, mutatePopulationPercent:Double): Population = {

    def populationPercentToNumber(populationPercent:Double): Int = {
      (populationPercent * population.length).toInt
    }

    val rest = BigDecimal(1.0 - crossPopulationPercent - mutatePopulationPercent)
      .setScale(1, BigDecimal.RoundingMode.HALF_UP)
      .toDouble

    new Population(
      cross(populationPercentToNumber(crossPopulationPercent))
      ++ mutate(populationPercentToNumber(mutatePopulationPercent))
      ++ fillWithRandom(populationPercentToNumber(rest))
      , fitness)
  }

  def getBest():Individual = {
    println("Getting best...")
    population.reduceLeft(_>_)
  }

  def printPopulation() :Unit = {
    population.foreach(it => println(it))
  }

}
