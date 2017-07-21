package techgig

/**
  * Created by jyothi on 11/7/17.
  */
object TechWeekChallenge4 extends App{

  def divideAndRule(input1: Array[Int]): Int = {
    var buffer1Sum = 0
    var buffer2Sum = 0
    input1.foreach(i =>{
        if(buffer1Sum < buffer2Sum){
          if(i > 0) {
            buffer1Sum += i
          }else{
            buffer2Sum += i
          }
        }else{
          if(i > 0) {
            buffer2Sum += i
          }else{
            buffer1Sum += i
          }
        }
    })
    if(input1.contains(buffer2Sum - buffer1Sum)){
      return 1
    }else if(input1.contains(buffer1Sum - buffer2Sum)){
      return 1
    }
    -1
  }

  /*println(divideAndRule(Array(2, 2, 2, 8, 7)))
  println(divideAndRule(Array(1, 1, 1, 1, 1)))
  println(divideAndRule(Array(1, 2, 3, 4, 5, 6, 5)))
  println(divideAndRule(Array(-1, -1, -1, -1, -1)))*/

}
