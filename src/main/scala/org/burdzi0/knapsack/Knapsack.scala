package org.burdzi0.knapsack

class Knapsack {

  var elements: List[KnapsackElement] = List[KnapsackElement]()

  def addKnapsackElement(knapsackElement: KnapsackElement): Unit = {
      elements = knapsackElement::elements
  }
}
