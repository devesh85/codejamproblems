package com.qualifyinground

import scala.io.Source

/**
 * Created by devesh on 4/10/15.
 */
object Dijikstra extends App{
  val sampleFile = "/Users/devesh/NextStep/codejamproblems/src/main/resources/dijikstra/small-input.txt"
  val list = Source.fromFile(sampleFile).getLines().toList
  val T =  list.head.toInt

  def defineTable(leftStr: String, rightStr: String):String = {
    leftStr match{
       case "1" => rightStr
       case "i" => rightStr match{
         case "1"=> "i"
         case "i"=> "-1"
         case "j"=> "k"
         case "k"=> "-j"
         case "-1"=> "-i"
         case "-i"=> "1"
         case "-j"=> "-k"
         case "-k"=> "j"
       }
       case "j" => rightStr match{
         case "1"=> "j"
         case "i"=> "-k"
         case "j"=> "-1"
         case "k"=> "i"
         case "-1"=> "-j"
         case "-i"=> "k"
         case "-j"=> "1"
         case "-k"=> "-i"
       }
       case "k" => rightStr match{
         case "1"=> "k"
         case "i"=> "j"
         case "j"=> "-i"
         case "k"=> "-1"
         case "-1"=> "-k"
         case "-i"=> "-j"
         case "-j"=> "i"
         case "-k"=> "1"
       }
       case "-1" => rightStr match{
         case "1"=> "-1"
         case "i"=> "-i"
         case "j"=> "-j"
         case "k"=> "-k"
         case "-1"=> "1"
         case "-i"=> "i"
         case "-j"=> "j"
         case "-k"=> "k"
       }
       case "-i" => rightStr match{
         case "1"=> "-i"
         case "i"=> "1"
         case "j"=> "-k"
         case "k"=> "j"
         case "-1"=> "i"
         case "-i"=> "-1"
         case "-j"=> "k"
         case "-k"=> "-j"
       }
       case "-j" => rightStr match{
         case "1"=> "-j"
         case "i"=> "k"
         case "j"=> "1"
         case "k"=> "-i"
         case "-1"=> "j"
         case "-i"=> "-k"
         case "-j"=> "-1"
         case "-k"=> "i"
       }
       case "-k" => rightStr match{
         case "1"=> "-k"
         case "i"=> "-j"
         case "j"=> "i"
         case "k"=> "1"
         case "-1"=> "k"
         case "-i"=> "j"
         case "-j"=> "-i"
         case "-k"=> "-1"
       }
     }

  }


  def accumulateRemaings(patternStr: String, lookForStr:String,currAcc: String, accumulator:List[String]):List[String] ={

    var checkStrHead = patternStr.head
    var remaining = patternStr.tail
    val newAcc = defineTable(currAcc, checkStrHead.toString)
    remaining match {
      case "" =>  accumulator
      case _ =>   if(newAcc == lookForStr)
                      remaining::accumulateRemaings(remaining, lookForStr, "1", accumulator)
                  else
                      accumulateRemaings(remaining, lookForStr, newAcc, accumulator)
    }
  }

  def findAllIMatches(patternStr:String):List[String] ={
      accumulateRemaings(patternStr, "i", "1", Nil)
  }

  def findAllJMatches(patternStr:String):List[String] ={
      accumulateRemaings(patternStr, "j", "1", Nil)
  }

  def findIfPatternMatchedK(patternStr: String):Boolean = {
    val strList = patternStr.toList.map(a => a.toString )
    strList.foldLeft("1")(defineTable) == "k"
  }


  def caseInput(str: String, times: Int):String={
    val patternStr = str * times
    val allPatternsAfterI = findAllIMatches(patternStr)
    allPatternsAfterI match {
      case x::xs =>{
        val allPatternsWithJ = for(str <- allPatternsAfterI) yield {findAllJMatches(str)}
        val flattened = allPatternsWithJ.flatten
        flattened match {
          case  x::xs =>  val isValid = flattened.foldLeft(false)(_ || findIfPatternMatchedK(_))
            if(isValid) "YES"
            else "NO"
          case Nil => "NO"
        }
      }
      case Nil => "NO"
    }


  }

//
//  def accumulateListForString(list: List[String], lookForStr:String,currAcc: String, accumulator:List[String]):List[String] ={
//
//  }
//
//  def newCaseInput(str:String , times:Int):String={
//    val list = createListFromData(str, times)
//
//  }

 def createListFromData(str:String , times:Int) :List[String]={
    val patternStr = str * times
    val charList = patternStr.toList
    val strList = charList.map( a => a.toString)

    def compressList(list: List[String], head:String, result:List[String]):List[String] ={
       list match{
         case x::xs => {
            if(head == "1" || head == "-1"){
                compressList(xs, defineTable(head,x), result)
            }else{
              val product = defineTable(head, x)
              if(product == "1" || product == "-1"){
                 compressList(xs, product, result)
              }else{
                 head::compressList(xs, x, result)
              }
            }
         }
         case Nil => head::result
       }
    }
    compressList(strList, "1", Nil)
 }



//  def processCases(inputs: List[String], result:List[String], caseIndex: Int):List[String]={
//    inputs match {
//        case x::y::rest =>{
//             val inputHead = x.split(" ")(1).toInt
//             val inputStr = y
//             val caseResult = "Case #" + caseIndex + ": " + caseInput(inputStr, inputHead)
//             caseResult::processCases(rest, result, caseIndex +1)
//        }
//        case Nil=> result
//      }
//  }
//
//
//
//  processCases(list.tail, Nil, 1).foreach(x => println(x))

}
