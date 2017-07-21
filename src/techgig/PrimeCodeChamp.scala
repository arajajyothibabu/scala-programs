package techgig

/**
  * Created by jyothi on 29/6/17.
  */
object PrimeCodeChamp extends App {

  def amount_value(input1: Array[String]): Array[String] = {
    val arr = new Array[Array[Int]](input1.length)
    val buffer = scala.collection.mutable.ListBuffer[(Int, Int, Int)]()
    for(i <- input1.indices){
      arr(i) = input1(i).split("#").map(_.toInt)
    }
    for(i <- arr.indices){
      for(j <- arr(i).indices){
        buffer.append((i, j, computeMin(i, j, arr)))
      }
    }
    val max = buffer.maxBy(_._3)._3
    buffer.filter(_._3 == max).toArray.map(el => s"${(el._1 + 1).toString}#${(el._2 + 1).toString}")
  }


  def computeMin(x: Int, y: Int, arr: Array[Array[Int]]): Int = {
    var min = arr(x)(y)
    val l = if(arr.length > 0) arr(0).length else 0
    val m = arr.length
    if(x > 0 && y > 0 && arr(x - 1)(y - 1) < min) min = arr(x - 1)(y - 1)
    if(x > 0 && arr(x - 1)(y) < min) min = arr(x - 1)(y)
    if(x > 0 && y < m - 1 && arr(x - 1)(y + 1) < min) min = arr(x - 1)(y + 1)
    if(y > 0 && arr(x)(y - 1) < min) min = arr(x)(y - 1)
    if(y < m - 1 && arr(x)(y + 1) < min) min = arr(x)(y + 1)
    if(x < l -1 && y > 0 && arr(x + 1)(y - 1) < min) min = arr(x + 1)(y - 1)
    if(x < l - 1 && arr(x + 1)(y) < min) min = arr(x + 1)(y)
    if(x < l -1 && y <  m - 1 && arr(x + 1)(y + 1) < min) min = arr(x + 1)(y + 1)
    min
  }

  def exec(args: Array[String]) {

    val ip1_size = scala.io.StdIn.readInt
    val ip1 = Array.ofDim[String](ip1_size)

    for (ip1_i <- 0 until ip1_size)
      ip1(ip1_i) = scala.io.StdIn.readLine


    val output = amount_value(ip1)
    println(output.mkString("\n"))


  }

  exec(Array.empty[String])
/*
12#45#33#27
94#54#23#53
98#59#27#62
11#51#67#13
*/
}
