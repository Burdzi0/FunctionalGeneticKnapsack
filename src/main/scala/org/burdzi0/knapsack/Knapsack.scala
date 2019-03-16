package org.burdzi0.knapsack

class Knapsack(knapsackElement: KnapsackElement*) {

  var elements: List[KnapsackElement] = knapsackElement.toList

  def addKnapsackElement(knapsackElement: KnapsackElement): Unit = {
      elements = knapsackElement::elements
  }
}
