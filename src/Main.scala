import scala.collection.mutable.ListBuffer
import scala.io.StdIn._
/**
  * Created by jyothi on 19/6/17.
  */
object Main extends App {
  var time = 0L
  var artifact = "0"

  def readInputs(testCase: Int): Unit = {
    val input = readLine().split(" ")
    println(s"Case #$testCase: ${newCheckS(input.head.toLong, input.last.toString)}")
  }

  def checkS(k: Long, target: String): Long = {
    var foundLastIndex = -1
    var lastIndex = artifact.indexOf(target, foundLastIndex)
    var times = if (lastIndex > -1) {
      foundLastIndex = lastIndex
      1
    } else 0
    while (times < k) {
      time = time + 1L
      artifact = artifact + time.toString
      lastIndex = artifact.indexOf(target, foundLastIndex + 1)
      if (lastIndex > -1) {
        foundLastIndex = lastIndex
        times = times + 1
      }
    }
    foundLastIndex
  }

  def newCheckS(k: Long, target: String): Long = {
    val limit = k * ("1" + target).toLong
    val seq = if(limit > Integer.MAX_VALUE){
      (0L to Integer.MAX_VALUE - 1).map(_.toString).mkString("")// + (Integer.MAX_VALUE.toLong to limit).map(_.toString).mkString("")
    }else{
      (0L to limit).map(_.toString).mkString("")
    }
    var times = 0
    var lastIndex = -1
    var foundLastIndex = lastIndex
    while(times < k) {
      lastIndex = seq.indexOf(target, foundLastIndex + 1)
      if (lastIndex > -1) {
        foundLastIndex = lastIndex
        times = times + 1
      }
    }
    foundLastIndex
  }

  /*val testCases = readInt()
  for(i <- 1 to testCases){
    readInputs(i)
  }*/

  def countForTensChars(n: Long): Long = {
    if(n > 9) {
      countForTensChars(n / 10) + 9 * tenthRoot(n) * n / 10 + 1
    } else n
  }

  def tenthRoot(n: Long): Int = {
    var inPowersOfTen = 1
    while(n / (10 * inPowersOfTen) > 0){
      inPowersOfTen = inPowersOfTen + 1
    }
    inPowersOfTen - 1
  }

  def countChars(n: Long): Long = {
    val power = tenthRoot(n)
    val tens = Math.pow(10, power).toLong
    if(tens == n) countForTensChars(n)
    else (power + 1) * (n - tens) + countForTensChars(tens)
  }

  //println(countChars(readLong()))

  //readInputs(1)

  def nextPalindrome(k: Int): Int = {
    var nextK = k + 1
    while(true){
      val bin = Integer.toString(nextK)
      if(bin == bin.reverse){
        return nextK
      }
      nextK = nextK + 1
    }
    nextK
  }

  def checkNextPalindromes(): Unit = {
    val t = readInt()
    (1 to t).foreach(int => {
      println(nextPalindrome(readInt()))
    })
  }

  //checkNextPalindromes()

  def uncommonBetweenCommon(input1: Array[Int], input2: Array[Int]): String = {
    //set1.diff(set2).union(set2.diff(set1)).toList.sorted.mkString("$")
    val result = scala.collection.mutable.ListBuffer[Int]()
    result.append(input1:_*)
    input2.foreach(el => {
      if(result.contains(el)){
        result.remove(result.indexOf(el))
      }else{
        result.append(el)
      }
    })
    result.sorted.mkString("$")
  }

  //println(uncommonBetweenCommon(Array(1, 2, 3, 5, 6), Array(1, 3)))

  def alternateSorting(input1: Array[Int]): Array[Int] = {
    val buffer = scala.collection.mutable.ListBuffer(input1.sortWith(_ > _):_*)
    val result = scala.collection.mutable.ListBuffer[Int]()
    println(buffer.size)
    var  x = 0
    while(buffer.size > 1 && x < 10){
      result.append(buffer.head)
      result.append(buffer.last)
      buffer.remove(buffer.size -1)
      buffer.remove(0)
      println(buffer.size)
      x = x + 1
    }
    if(buffer.size == 1){
      result.append(buffer.head)
    }
    result.toArray
  }

  //println(alternateSorting(Array(1, 2, 3, 4, 5, 6, 7)))

  def nochange_bits(input1: String, input2: Int, input3: Int): Int = {
    if(input1.length < input2 || input1.length < input3 || (input2 * input3 < 0) || !input1.matches("[01]+")){
      -1
    }else{
      (1 to input1.length).map(i => {
        if(i % input2 == 0 && i % input3 == 0) 1
        else if(i % input2 == 0) 0
        else if(i % input3 == 0) 0
        else 1
      }).sum
    }
  }

  /*println(nochange_bits("10110101101", 3, 4))
  println(nochange_bits("101101011023", 3, 4))
  println(nochange_bits("1111", 3, 4))*/
  //println(nochange_bits("101101001", 2, 3))

  def ThirstyCrowProblem(input1: Array[Int], input2: Int): Int = {
    if(input1.length < input2 || input2 < 0 || input1.min < 0) return -1
    var potsNeed = input2
    var stonesThrown = 0
    val potBuffer = scala.collection.mutable.ListBuffer[Int](input1:_*)
    while(potsNeed > 0){
      val min = potBuffer.min
      stonesThrown = min * potBuffer.size
      potBuffer.remove(potBuffer.indexOf(min))
      potsNeed = potsNeed - 1
      while(min == potBuffer.min){
        potBuffer.remove(potBuffer.indexOf(min))
        potsNeed = potsNeed - 1
      }
    }
    stonesThrown
  }

  /*println(ThirstyCrowProblem(Array(0, 58), 1))
  println(ThirstyCrowProblem(Array(5, 58), 1))
  println(ThirstyCrowProblem(Array(5, 58), -1))
  println(ThirstyCrowProblem(Array(5, 58, 5, 54), 3))
  println(ThirstyCrowProblem(Array(5, 58, 5, 54, -1), 3))*/

  def FACT(): Unit = {
    val n = scala.io.StdIn.readInt
    1 to n foreach(i => println(Z(scala.io.StdIn.readInt)))
  }

  def Z(n: Int): Int = {
    var tempN = n
    var zeros = 0
    while(tempN / 5 > 0){
      zeros = zeros + tempN / 5
      tempN = tempN / 5
    }
    zeros
  }

  def factorial(n: Long) : Long = {
    if(n > 0) n * factorial(n - 1)
    else 1
  }

  //FACT()

  /*1 to readInt foreach(i => {
    val in = readLine.split(" ")
    primesBetween(in.head.toInt, in.last.toInt)
    println()
  })*/

  def primesBetween(a: Int, b: Int): Unit = {
    val buffer = new Array[Boolean](b).map(el => true)
    var p = 2
    while(p * p < b){
      if(buffer(p)){
        var i = p * 2
        while(i < b){
          buffer(i) = false
          i += p
        }
      }
      p += 1
    }
    buffer.zipWithIndex.foreach(el => if(el._1) println(el._2))
  }


}