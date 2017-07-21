import scala.collection.mutable.ListBuffer
import scala.language.reflectiveCalls

/**
  * Created by jyothi on 26/1/17.
  */
object Solution extends App{

  override def main(args: Array[String]) {

    val x = List(1, 2, 3, 4, 5)
    val y = List(3, 4, 1, 2, 3, 4, 1, 2, 3)
    //println(getCommonSubSequenceFromStart(x, y))

    //fun1(Seq(1, 2, 3))

  }

  def getCommonSubSequenceFromStart[T](source: List[T], target: List[T]): List[T] = {
    val result = new ListBuffer[ListBuffer[T]]
    val targetIterator = target.toIterator
    var sourceIterator = source.toIterator
    var buffer = new ListBuffer[T]()
    while(sourceIterator.hasNext && targetIterator.hasNext){
      val key = sourceIterator.next
      val targetKey = targetIterator.next
      if(key == targetKey){
        buffer.append(key)
        if(!targetIterator.hasNext || !sourceIterator.hasNext){
          result.append(buffer)
        }
      }else{
        result.append(buffer)
        sourceIterator = source.toIterator
        buffer = new ListBuffer[T]()
        if(sourceIterator.next == targetKey){
          buffer.append(targetKey)
        }
      }
    }
    result.maxBy(list => list.size).toList
  }

  def fun1[T <: { def toDouble:Double }] (seq:Seq[T]) = {
    val q2 = seq map { _.toDouble }
    val n = q2.size.toDouble
    q2.sum / n
  }

}