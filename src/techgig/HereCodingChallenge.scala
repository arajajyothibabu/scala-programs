package techgig

import scala.collection.mutable

/**
  * Created by jyothi on 21/8/17.
  */

/**
  * Attack on Base stations (100 Marks)

All the cities of a state (call it state1) are connected to each other via roads, i.e. pick any two pairs of cities, one can reach one city from the other one via some path. Two of its cities are the army base stations.
An enemy state (call it state2) plans to attack on this state but it can t attack on both the army bases of state1 simultaneously. Therefore, it wants to disconnect both the army base of state1 and conquer them one by one. This can be done by destroying some of the roads between the cities.
Given a road network of the state, find minimum no. of roads that need to be destroyed so that the state is divided into two sets(non-empty) of cities which are not reachable from each other( or in other words, leaving both the base stations unconnected from each other). Each city can be expressed as an integer and cities with largest and smallest number are the army base stations.

Input Format
You will be given a function which contains a string array as argument representing. List of all roads { list of integer pairs x#y : i.e there is a road from city x to city y }

Output Format
You need to return the Minimum number of roads to be destroyed in the form of integer from the given function


Sample Test Case
Sample Input
7
1#2
1#5
2#5
2#3
3#4
4#5
4#6

Sample Output
1


Explanation
Consider the following road network(1 and 6 are base stations):
 Input as a string array {1#2,1#5,2#5,2#3,3#4,4#5,4#6} for this example.



Destroying the roads (4#5) and (3#2) will disconnect the two base stations leaving no path to reach 6 from 1. But there is another better way possible to do so. We can disconnect 1 and 6 by destroying only one road(4,6) as shown below:


Hence your answer should be 1 (minimum number of roads destroying which, two base stations can be disconnected).
  */

object HereCodingChallenge extends App {

  case class Road(start: String, next: List[Road])

  import scala.collection.mutable.{ListBuffer, HashMap}
  def minRoads(input1: Array[String]): Int = {
    val delimiter = "#"
    val cityWithRoads = mutable.HashMap[String, ListBuffer[String]]()
    val cities = input1.flatMap(link => {
      val endCities = link.split(delimiter)
      if(!cityWithRoads.isDefinedAt(endCities.head)){
        cityWithRoads.put(endCities.head, ListBuffer(endCities.last))
      }else{
        val buffer = cityWithRoads.apply(endCities.head)
        buffer.append(endCities.last)
        cityWithRoads.update(endCities.head, buffer)
      }
      endCities.map(_.toInt)
    }).toSet
    val baseA = cities.min
    val baseB = cities.max
    val roads = input1.map(_.split(delimiter))
    println(cities.mkString("-"))
    println(baseA)
    println(baseB)
    println(isConnected(cityWithRoads, baseA.toString, baseB.toString))
    0
  }

  def isConnected(roads: mutable.HashMap[String, ListBuffer[String]], cityA: String, cityB: String): Boolean = {
    val isCity = roads.get(cityA)
    if(isCity.isDefined){
      val connectedCities = isCity.get
      if(connectedCities.contains(cityB)){
        true
      }else{
        for(city <- connectedCities){
          val connectionFlag = isConnected(roads, city, cityB)
          if(connectionFlag) return true
        }
        false
      }
    }else{
      false
    }
  }

  minRoads(Array("1#2", "1#5", "2#5", "2#3", "3#4", "4#5", "4#6", "5#12", "3#13", "4#14", "5#15", "7#17"))

}
