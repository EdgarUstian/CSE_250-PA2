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
    //Init new Node
    var node: DNode[TaxEntry] = new DNode[TaxEntry](taxEntry, null, null)
    //Extract the grouping attribute
    val taxAttribute = taxEntry.infoMap(groupingAttribute)
    //Increment length
    numStored += 1
    //Empty Case
    if(groupings.isEmpty){
      groupings.append(node)
    }
    //Non-Empty Case
    else{
      var indX: Int = 0
      var stop: Boolean = false
      //Iterate groupings and find the value
      while(indX < groupings.length && !stop){
        //Extract grouping attribute
        val groupAttribute: String = groupings(indX).value.infoMap(groupingAttribute)
        //If found
        if(taxAttribute == groupAttribute){
          groupings(indX).prev = node
          node.next = groupings(indX)
          groupings(indX) = node
          stop = true
        }
        //Not Found
        //The attribute is greater
        else if(taxAttribute > groupAttribute){
            indX += 1
        }
        //The attribute is less
        else if(taxAttribute < groupAttribute){
          groupings.insert(indX, node)
          stop = true
        }
      }
      //Reached the end (greatest value)
      if(indX >= 0 && !stop){
        groupings.insert(indX, node)
      }
    }
  }

  /** Regroup . */
  def regroup(attribute: String): Unit = {
    numStored = 0
    groupingAttribute = attribute
    var tempList: List[TaxEntry] = List()
    for(indX <- groupings.indices){
      var node = groupings(indX)
      while(node.next != null){
        tempList = tempList :+ node.value
        node = node.next
      }
      tempList = tempList :+ node.value
    }
    groupings.clear()
    for(entry <- tempList){
      insert(entry)
    }
  }

  /** Returns an Iterator to all entries that can be used only once. */
  def iterator: Iterator[TaxEntry] = new Iterator[TaxEntry] {
    //Start Iterator at head
    private var indX: Int = 0
    private var current: DNode[TaxEntry] = _
    if(groupings.nonEmpty){
      current = groupings(indX)
    }

    override def hasNext: Boolean = current != null && groupings.nonEmpty

    override def next(): TaxEntry = {
      val placeholder: DNode[TaxEntry] = current
      if(current.next != null){
        current = current.next
        placeholder.value
      }
      //Reached the end of grouping
      else{
        if(indX + 1 < groupings.size){
          indX += 1
          current = groupings(indX)
          placeholder.value
        }
        //Reaches end of list
        else{
          current = null
          placeholder.value
        }
      }
    }
  }
  
  /** Returns an Iterator to only the entries with matching values on the grouping attribute that can be used only once. */
  def iterator(value: String): Iterator[TaxEntry] = new Iterator[TaxEntry] {
    //Start at the head of matching value
    private var current: DNode[TaxEntry] = new DNode[TaxEntry](null, null, null)
    private var indX: Int = 0
    private var exists: Boolean = false
    while(indX < groupings.size && !exists){
      //Extract grouping attribute
      val groupAttribute: String = groupings(indX).value.infoMap(groupingAttribute)
      if(value == groupAttribute){
        exists = true
      }
      indX += 1
    }
    /*
    * Function of the while loop is to find out if the desired category exists
    * If while loop finishes while exists is still false, it is not there
     */
    override def hasNext: Boolean = exists && groupings.nonEmpty

    if(hasNext){
      current = groupings(indX - 1)
    }

    override def next(): TaxEntry = {
      val placeholder: DNode[TaxEntry] = current
      if(current.next != null){
        current = current.next
        placeholder.value
      }
      //Reached the end of grouping
      else{
        exists = false
        placeholder.value
      }
    }
  }

  def length: Int = numStored

  override def toString: String = if (numStored == 0) "" else this.iterator.addString(new StringBuilder, "\n").result()
}
