package com.qualifyinground

import scala.io.Source

/**
 * Created by devesh on 4/12/15.
 */
object Dijikstra2 extends App{
  val sampleFile = "/Users/devesh/NextStep/codejamproblems/src/main/resources/dijikstra/small-input.txt"
  val list = Source.fromFile(sampleFile).getLines().toList
  val T =  list.head.toInt
  def processCases(inputs: List[String], result:List[String], caseIndex: Int):List[String]={
      inputs match {
          case x::y::rest =>{
               val inputHead = x.split(" ")(1).toInt
               val inputStr = y
               println(caseIndex)
               val caseResult = "Case #" + caseIndex + ": " + caseInput(inputStr, inputHead)
               caseResult::processCases(rest, result, caseIndex +1)
          }
          case Nil=> result
        }
    }
    processCases(list.tail, Nil, 1).foreach(x => println(x))

  def caseInput(str: String, times: Int): String = {
    val patternStr = str * times
    val charList = str.toList
    val strList = charList.map( a => a.toString)
    val curVal = strList.foldLeft("1")((B,A) => multiply(B, A))


   //run currval from 1, times%4 times start with 1
//    val allMul = List.range(1, (times%4)+1).foldLeft("1")((B,A) => multiply(B, A))
//    allMul match  {
//      case "-1" => {
//         "YES"
//      }
//      case _ => "NO"
//    }
     ""
  }

  def abs(a:String):String = {
    a.charAt(0) match {
      case '0' => a.charAt(1).toString
      case _ => a
    }
  }
  def multiply(first:String, second:String):String ={
    val a = abs(first)
    val b = abs(second)
    val tmpMul = a match {
      case "1" => b
      case b => "-1"
      case "i" => {
        b match {
          case "j" => "k"
          case "k" =>  "-j"
        }
      }
      case "j" => {
        b match {
          case "i" => "-k"
          case "k" =>  "i"
        }
      }
      case "k" => {
        b match {
          case "i" => "j"
          case "j" =>  "-i"
        }
      }
    }

    val isAnsPos = (first == a && second == b) || (first != a && second != b)
    isAnsPos match {
      case true => multiply("-1", tmpMul)
      case false => tmpMul
    }
  }
}
