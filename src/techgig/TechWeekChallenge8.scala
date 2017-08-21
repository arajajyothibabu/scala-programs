package techgig

/**
  * Created by jyothi on 19/8/17.
  */
object TechWeekChallenge8 extends App {

  def getJoinedPipes(input1: Array[Int]): Array[Int] = {
    if(input1.length == 1){
      Array(0)
    }else{
      val buffer = scala.collection.mutable.ListBuffer.apply(input1:_*)
      val out = scala.collection.mutable.ListBuffer[Int]()
      out.append(buffer.min)
      buffer.remove(buffer.indexOf(buffer.min))
      while(buffer.nonEmpty){
        out.append(out.last + buffer.min)
        buffer.remove(buffer.indexOf(buffer.min))
      }
      out.remove(0)
      out.toArray
    }
  }

  println(getJoinedPipes(Array(2, 3, 4, 6)).mkString("-"))
  println(getJoinedPipes(Array(2, 3, 4, 6, 6, 4, 2)).mkString("-"))
  println(getJoinedPipes(Array(2, 3, 4)).mkString("-"))
  println(getJoinedPipes(Array(2)).mkString("-"))

}
