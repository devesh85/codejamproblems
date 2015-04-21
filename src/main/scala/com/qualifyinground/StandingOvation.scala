package com.qualifyinground

import scala.io.Source

/**
 * Created by devesh on 4/10/15.
 */
object StandingOvation extends App{
//  val sampleFile = "/Users/devesh/NextStep/codejamproblems/src/main/resources/standingovation/sample-input.txt"
//  val sampleFile = "/Users/devesh/NextStep/codejamproblems/src/main/resources/standingovation/small-input.txt"
  val sampleFile = "/Users/devesh/NextStep/codejamproblems/src/main/resources/standingovation/large-input.txt"
  val list = Source.fromFile(sampleFile).getLines().toList

  val T = list.head.toInt

  def caseInput(str:String):Int ={
      val strArr = str.split(" ")
      val audience = strArr(1)
//      println( audience)

      def getRequiredPeople(index: Int, values: List[Char], standing:Int, result: Int): Int ={
          values match {
            case x::xs => {
              val peopleNeeded = index
              val people = x - '0'
              if(peopleNeeded > standing && people > 0){
                getRequiredPeople(index + 1, xs , peopleNeeded + people,result + (peopleNeeded - standing))
              }else{

                getRequiredPeople(index + 1, xs ,standing + people,result)
              }

            }
            case Nil => result
          }
      }
//      println (getRequiredPeople (0, audience.toList, 0))
      return getRequiredPeople (0, audience.toList, 0, 0)
  }

  val cases = for(s <- list.tail) yield caseInput(s)

  def printCases(cases:List[Int], index: Int):Unit={
      cases match {
         case x::xs => println ("Case #" + index + ": " + x)
            printCases(xs, index +1)
         case Nil =>
            println ("")
      }
  }

  printCases(cases, 1)
}
