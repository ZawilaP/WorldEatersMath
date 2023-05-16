import Domain._

import scala.util.Random

object Main extends App {
  private val nSamples = 1000000
  private val nRolls = 8

  case class Roll(value: Map[DiceValue, Count]) {
    def check(power: Powers): Boolean = power.check(value)
  }

  private object Roll {
    def apply(value: List[Int]): Roll = {
      Roll(
        value
          .groupBy(identity)
          .map { case (number, occurrences) => number -> occurrences.size}
      )
    }
  }

  private case class AllRolls(rolls: List[Roll]) {
    def summary: Map[String, Double] = {
      val rollsLength = rolls.length.toDouble
      rolls.flatMap(x =>
        Map(
          RageFuelledInvigoration -> x.check(RageFuelledInvigoration),
          WrathfulDevotion -> x.check(WrathfulDevotion),
          MartialExcellence -> x.check(MartialExcellence),
          TotalCarnage -> x.check(TotalCarnage),
          WarpBlades -> x.check(WarpBlades),
          UnbridledBloodlust -> x.check(UnbridledBloodlust)
        )).groupBy(_._1)
        .map { case (key, pairs) =>
          val probability: Double = pairs.count(_._2) / rollsLength
          key.toString -> probability
        }
    }
  }

  private val rollGen: List[Roll] = List.fill(nSamples)(Roll(List.fill(nRolls)(new Random().nextInt(6) + 1)))

  println(AllRolls(rollGen).summary)
}