package com.qualifyinground

import scala.io.Source

/**
 * Created by devesh on 4/10/15.
 */
object HouseOfPancakes extends App{
  val sampleFile = "/Users/devesh/NextStep/codejamproblems/src/main/resources/houseofpancakes/small-input.txt"
//  val sampleFile = "/Users/devesh/NextStep/codejamproblems/src/main/resources/houseofpancakes/sample-input.txt"
  val list = Source.fromFile(sampleFile).getLines().toList
  val T =  list.head.toInt

  def minutesEatesVsWait(pancakeNum: Int, currMap: Map[Int, (Int, Int)]):Map[Int, (Int, Int)]={
    val keys = currMap.keySet
    currMap
  }

  def caseInput(pancakes: List[Int]):Int={
    def processPancakes(currPancakes: List[Int], minutesElapsed:Int):Int={
      val maxP = currPancakes.max
      maxP match {
        case 0 => minutesElapsed
        case 1 => minutesElapsed +1
        case 2 => minutesElapsed +2
        case 3 => minutesElapsed + 3
        case 4 => minutesElapsed + 3
        case _ => {
           minutesElapsed
        }
      }
    }

    val pancakeMap = pancakes.groupBy(identity).mapValues(_.size)
    processPancakes(pancakes, 0)
  }

  def updateMap( pancakeId :Int):(Int,Int,Int, Int)={
      pancakeId match {
        case 0 => (0,0,0,0)
        case 1 => (1,1,0,1)
        case 2 =>  (2,2,0,2)
        case _ => {
          val splitPairs = List.range(1, pancakeId/2+1).map(a => (a, pancakeId -a))
          val splitMinutesList = splitPairs.map(tupPair => (updateMap(tupPair._1), updateMap(tupPair._2)))
          val newUpdateVals = splitMinutesList.map( tupRes => {
            val firstMin = tupRes._1._4
            val secondMin = tupRes._2._4
            if(firstMin > secondMin){
              (pancakeId, tupRes._1._1, tupRes._2._1, firstMin+1)
            }else{
              (pancakeId, tupRes._1._1, tupRes._2._1, secondMin+1)
            }
          } )

          val allList = (pancakeId, pancakeId, 0, pancakeId)::newUpdateVals
          allList.min(Ordering.by((tup:(Int,Int, Int,Int)) => tup._4))
        }
      }
  }


  //(Int, Int, Int) => First, Second, Minutes
//  def updateMap (pancakeId: Int, accMap:Map[Int, (Int, Int, Int)]):Map[Int, (Int, Int, Int)]={
//     pancakeId match {
//       case 0 => accMap + (0 -> (0,0,0))
//       case 1 => accMap + (1 -> (1,0,1))
//       case 2 => accMap + (2 -> (2, 0, 2))
//       case _ =>{
//         val splitPairs = List.range(1, pancakeId/2).map(a => (a, pancakeId -a))
////         val updatedMap = splitPairs.foldLeft(accMap)((mp,tup) => mp + updateMap(tup._1, mp) + updateMap(tup._2, mp))
////
//         val minutesList = splitPairs.map( tup => {
//                val minutesMap1 = updateMap(tup._1, accMap).get(tup._1)
//                val minutesMap2 = updateMap(tup._2, accMap).get(tup._2)
//
//         })
//       }
//     }
//  }
//
//  def updateMap(pancake: Int, currMap:Map[Int, (Int, Int)]):Map[Int, (Int, Int)]={
//    val tup = currMap.get(pancake)
//    tup match {
//      case Some(tup) => currMap
//      case None => {
//        val others = List.range(1, pancake-1).filter( _>0)
//
//        val splits = others.map( a=> (a,pancake -a))
//        val pairTupes = splits.map( t =>  updateMap(t._1, currMap ))
//      }
//    }
//  }

  def bruteForce(pancakes:List[Int], currMap:Map[Int, (Int, Int, Int, Int)],minutesElapsed: Int):Int ={
    val maxP = pancakes.max
    maxP match {
      case 0 => minutesElapsed
      case 1 => minutesElapsed + 1
      case 2 => minutesElapsed +2
      case 3 => minutesElapsed + 3
      case _ => {
        val (left,right) = pancakes.span(_ != maxP)
        val tupRs = currMap.get(maxP)
        tupRs match {
          case Some(value) =>{
            val otherMin =   bruteForce(value._2::value._3::left:::right.tail, currMap, minutesElapsed+1)
            val regularMin = bruteForce(pancakes.map(a=> a-1).filter(_ > 0), currMap,minutesElapsed +1)
              List(regularMin, otherMin).min
          }
          case None => bruteForce(pancakes.map(a=> a-1).filter(_ > 0), currMap,minutesElapsed +1)
        }

      }
    }
  }

  def processCases(inputs: List[String], result:List[String], caseIndex: Int):List[String]={
      inputs match {
          case x::y::rest =>{
               val inputHead = x.toInt
               val inputList = y.split(" ").toList.map(a=>a.toInt)
               val maxValue = inputList.max
               val currList = (List.range(1,maxValue+1).map(a=>updateMap(a)))

               val currMap:Map[Int, (Int, Int, Int, Int)] = currList map (t => t._1 -> t) toMap;
               println(currMap)
               val caseResult = "Case #" + caseIndex + ": " + bruteForce(inputList, currMap,0)

            caseResult::processCases(rest, result, caseIndex +1)
//              result
          }
          case Nil=> result
        }
  }



//    processCases(list.tail, Nil, 1).foreach(x => println(x))

  println(updateMap(9))
}
