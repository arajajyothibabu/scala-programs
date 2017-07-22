package techgig

/**
  * Created by jyothi on 22/7/17.
  */
object TechWeekChallenge6 extends App{

  def farms_division(input1: Array[Int]): String = {
    val yes = "yes"
    val no = "no"
    val invalid = "invalid"
    for(i <- input1){
      if(i <= 0) return invalid
    }
    val sum = input1.sum
    if(sum % 2 != 0){
      no
    }else{
      val halfSum = sum / 2
      for(i <- Combine.linear(input1)){
        if(i.length < input1.length && i.sum == halfSum){
          return yes
        }
      }
      no
    }
  }

  object Combine {
    def linear[A](xs: Seq[A]): Seq[Seq[A]] =
      xs match {
        case Seq() => Seq(Seq())
        case Seq(h, t @ _*) =>
          linear(t) flatMap { ts => Seq(ts, h +: ts) }
      }

    def binary[A](xs: Seq[A]): Seq[Seq[A]] =
      xs match {
        case Seq() => Seq(Seq())
        case Seq(x) => Seq(Seq(), Seq(x))
        case xs =>
          val (ls, rs) = xs.splitAt(xs.length / 2)
          for (l <- binary(ls); r <- binary(rs)) yield l ++ r
      }
  }

  println(farms_division(Array(2, 2)))
  println(farms_division(Array(4, 3, 3, 3)))
  println(farms_division(Array(3, 7, 5, 8, 4, 3)))
  println(farms_division(Array(4, 3, 3, 3, -1)))
  println(farms_division(Array(4, 3, 3, 3, 4)))

}
