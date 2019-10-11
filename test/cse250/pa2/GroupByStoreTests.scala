package cse250.pa2

import cse250.objects.TaxEntry
import org.scalatest.{BeforeAndAfter, FlatSpec}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class GroupByStoreTests extends FlatSpec with BeforeAndAfter {

  def loadEntries(filename: String, numToLoad: Int): ArrayBuffer[TaxEntry] = {
    // Scala Cookbook reading CSV:
    // https://www.oreilly.com/library/view/scala-cookbook/9781449340292/ch12s06.html
    val bufferedSource = io.Source.fromFile(filename)
    val lines = bufferedSource.getLines
    val buffer = new mutable.ArrayBuffer[TaxEntry]()
    lines.next
    for (_ <- 1 to numToLoad) {
      val line = lines.next
      val cols = line.split(',')
      var colsIndex = 0
      val taxEntry = new TaxEntry
      for (headerIndex <- TaxEntry.HEADERS.indices) {
        val entry = {
          if (colsIndex < cols.length && cols(colsIndex).length != 0 && cols(colsIndex)(0) == '"') {
            val sb = new StringBuilder
            sb ++= cols(colsIndex)
            sb += ','
            while (cols(colsIndex).length == 0 || cols(colsIndex).last != '"') {
              colsIndex += 1
              sb ++= cols(colsIndex)
              sb += ','
            }
            (TaxEntry.HEADERS(headerIndex), sb.result.dropRight(1))
          }
          else if (colsIndex < cols.length) {
            (TaxEntry.HEADERS(headerIndex), cols(colsIndex))
          }
          else {
            (TaxEntry.HEADERS(headerIndex), "")
          }
        }
        colsIndex += 1
        taxEntry.infoMap.addOne(entry)
      }
      buffer.append(taxEntry)
    }
    bufferedSource.close
    buffer
  }

  val smallFilename = "data/2017-2018_Assessment_Roll-updated-small.csv"
  val largeFilename = "data/2017-2018_Assessment_Roll-updated.csv"
  val maxCapacitySmallFile = 25
  var dataStore: GroupByStore = _


  before {
    dataStore = new GroupByStore
  }

  behavior of "GroupByStore.length"
  it should "be 0 when initialized" in {
    assert(dataStore.length == 0)
  }

  it should "be updated after each insertion" in {
    val entries = loadEntries(smallFilename, maxCapacitySmallFile)
    for (i <- entries.indices) {
      dataStore.insert(entries(i))
      assert(dataStore.length == i + 1)
    }
  }

  behavior of "GroupByStore.iterator"
  it should "make hasNext return false when first initialised" in {
    val store = new GroupByStore
    assert(!store.iterator.hasNext)
  }


  it should "retrieve all stored entries" in {
    val entries = loadEntries(smallFilename, maxCapacitySmallFile)
    val testEntriesSet = new mutable.HashSet[TaxEntry]
    
    // Add all loaded values into your dataStore.
    for (i <- entries.indices) {
      dataStore.insert(entries(i))
      testEntriesSet.add(entries(i))
      assert(dataStore.length == i + 1)
    }

    // Check that all loaded values are iterated through in your dataStore.
    val dataIterator = dataStore.iterator
    val storedEntriesSet = new mutable.HashSet[TaxEntry]
    for (_ <- entries.indices) {
      val taxEntry = dataIterator.next
      // Check that entry was in the set of inserted entries.
      assert(testEntriesSet.contains(taxEntry))
      // Check that all entries are unique.
      assert(!storedEntriesSet.contains(taxEntry))
      storedEntriesSet.add(taxEntry)
    }
    assert(!dataIterator.hasNext)
  }

  behavior of "GroupByStore.iterator(String)"
  it should "retrieve all stored entries matching the value in the string" in {
    val entries = loadEntries(smallFilename, maxCapacitySmallFile)


    // Add all loaded values into your dataStore.
    for (i <- entries.indices) {
      dataStore.insert(entries(i))
      assert(dataStore.length == i + 1)
    }

    // Check that all loaded values are iterated through in your dataStore.
    val valueArray = Array("AUBURN", "MAIN")
    val matchIndicesArray = Array(Array(6,10,22,23), Array(1,5,12))
    for (testNum <- valueArray.indices) {
      // Value to match on.
      val value = valueArray(testNum)

      // Add expected entries to testEntriesSet.
      val testEntriesSet = new mutable.HashSet[TaxEntry]
      for(i <- matchIndicesArray(testNum)) testEntriesSet.add(entries(i))

      // Add entries found when iterating through group.
      val dataIterator = dataStore.iterator(value)
      val storedEntriesSet = new mutable.HashSet[TaxEntry]
      for (_ <- 0 until testEntriesSet.size) {
        assert(dataIterator.hasNext)
        val taxEntry = dataIterator.next
        // Check that entry was in the set of expected entries.
        assert(testEntriesSet.contains(taxEntry))
        // Check that all entries are unique.
        assert(!storedEntriesSet.contains(taxEntry))
        storedEntriesSet.add(taxEntry)
      }
      assert(!dataIterator.hasNext)
    }
  }
}
