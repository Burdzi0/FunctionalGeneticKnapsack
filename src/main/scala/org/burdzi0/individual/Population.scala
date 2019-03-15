package org.burdzi0.individual

import scala.util.Random

class Population(val population: List[Individual]) {

  val rnd = new Random

  def cross(): List[Individual] = {
    population
      .map(ind => ind.cross(chooseRandom(), rnd.nextInt(ind.bits.length)))
  }

  def chooseRandom(): Individual = {
    population(rnd.nextInt)
  }
}
