/**
 * GroupByStore.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: edgarust
 * Person#: 50230866
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT: edgarust
 */
package cse250.pa2

import cse250.objects.{DNode, TaxEntry}
import collection.mutable.ArrayBuffer
import util.control.Breaks._

class GroupByStore {
  // Feel free to change the default value of groupings and modify it to val/var.
  private var groupings: ArrayBuffer[DNode[TaxEntry]] = new ArrayBuffer[DNode[TaxEntry]]
  private var groupingAttribute = "STREET"
  private var numStored = 0

  /** Inserts element to head of corresponding grouping list. */
  def insert(taxEntry: TaxEntry): Unit = {
    
  }

  /** Regroup . */
  def regroup(attribute: String): Unit = ???

  /** Returns an Iterator to all entries that can be used only once. */
  def iterator: Iterator[TaxEntry] = new Iterator[TaxEntry] {
    override def hasNext: Boolean = ???

    override def next(): TaxEntry = ???
  }
  
  /** Returns an Iterator to only the entries with matching values on the grouping attribute that can be used only once. */
  def iterator(value: String): Iterator[TaxEntry] = new Iterator[TaxEntry] {
    override def hasNext: Boolean = ???

    override def next(): TaxEntry = ???
  }

  def length: Int = numStored

  override def toString: String = if (numStored == 0) "" else this.iterator.addString(new StringBuilder, "\n").result()
}
