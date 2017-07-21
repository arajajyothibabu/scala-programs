package techgig

/**
  * Created by jyothi on 12/7/17.
  */
object CodeBattle extends App{

  def fibonacci_series(input1: Int, input2: Int): Array[Int] = {
    val out = new Array[Int](10)
    for(i <- out.indices){
      if(i > 1){
        out(i) = out(i - 1) + out(i - 2)
      }else if(i == 1){
        out(i) = input2
      }else{
        out(i) = input1
      }
    }
    out
  }

  def nextFeb(in: Int): Int = {
    if(in == 0 || in == 1) 1
    else nextFeb(in - 1) + nextFeb(in - 2)
  }

  println(fibonacci_series(1, 2).mkString)

}
