package org.burdzi0.individual

import org.burdzi0.knapsack.Knapsack

import scala.util.Random
import scala.math.abs

class Individual(val bits:List[Boolean], knapsack: Knapsack) {

  val rnd = new Random()

  def mutate(fitness:Int) : Individual = {
    var mutRes = mutateSingleBit(randomAbsolute(bits.length))
    1.to(5)
      .iterator
      .takeWhile(_ => mutRes.calculateFitness() > fitness)
      .foreach(_ => mutRes = mutateSingleBit(randomAbsolute(bits.length))
    )

    if (mutRes.calculateFitness() > fitness) {
      new Individual(bits.map(_ => false), knapsack)
    }
    mutRes
  }

  def mutateSingleBit(bit:Int): Individual = {
    new Individual(this.bits.updated(bit, !bits(bit)), knapsack)
  }

  def cross(individual: Individual): List[Individual] = {
    val crossingPlace = randomAbsolute(this.bits.length)

    def cutBitsLists(first: Individual, second: Individual): List[Boolean] = {
      first.bits.take(crossingPlace) ++ second.bits.drop(crossingPlace)
    }

    new Individual(cutBitsLists(this, individual), knapsack)::
    new Individual(cutBitsLists(individual, this), knapsack)::Nil
  }

  def +(individual: Individual) : List[Individual] = {
    this.cross(individual)
  }

  def calculateFitness():Int = {
    bits.zip(knapsack.elements).map(tuple => if (tuple._1) tuple._2.weight else 0).sum
  }

  def >(individual: Individual): Individual = {
    if (this.calculateFitness() > individual.calculateFitness()) this
    else individual
  }

  def randomAbsolute(bound:Int): Int = {
    abs(rnd.nextInt(bound))
  }

  override def toString: String = {
    "[Bits]: " + bits.map(b => if (b) 1 else 0).toString() +
    "[F]: " + calculateFitness()
  }
}
