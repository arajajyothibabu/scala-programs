package techgig

/**
  * Created by jyothi on 21/7/17.
  */
object TechWeekChallenge5 extends App{


  def FindingMoves(input1: Int): Int = {
    if(input1 < 1){
      return 1
    }
    ((1 to input1 + 1).map(i => 8 * i).sum + 1).toInt
  }

  println(FindingMoves(-1))
  println(FindingMoves(0))
  println(FindingMoves(1))
  println(FindingMoves(2))
  println(FindingMoves(3))

}
