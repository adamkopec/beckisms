#!/usr/bin/env -S scala-cli -S 3

package name.kopec.adam

object Chains {
  val votes = Seq(
    ("scala", 1),
    ("java", 4),
    ("scala", 10),
    ("scala", 1),
    ("python", 10),
    ("lisp", 100)
  )

  object Option1 {
    def votingWinner: String = votes
      .groupBy(_._1)
      .map { case (which, counts) =>
        (which, counts.foldLeft(0)(_ + _._2))
      }
      .toSeq
      .sortBy(_._2)
      .reverse
      .headOption
      .map(_._1)
      .getOrElse("lisp")
  }

  object Option2 {
    def votingWinner: String = {
      val votesByLang = votes groupBy { case (lang, _) => lang }
      val sumByLang = votesByLang map { case (lang, counts) =>
        val countsOnly = counts map { case (_, count) => count }
        (lang, countsOnly.sum)
      }
      val orderedVotes = sumByLang.toSeq.sortBy { case (_, count) =>
        count
      }.reverse

      val simplyTheBest = orderedVotes.headOption.map { case (lang, _) => lang }

      simplyTheBest.getOrElse("lisp")
    }
  }

  object Option3 {

    private def groupByLang(
        what: Seq[(String, Int)]
    ): Map[String, Seq[(String, Int)]] = what.groupBy { case (lang, _) =>
      lang
    }
    private def sumByLang
        : Map[String, Seq[(String, Int)]] => Seq[(String, Int)] =
      _.map { case (lang, counts) =>
        val countsOnly = counts map { case (_, count) => count }
        (lang, countsOnly.sum)
      }.toSeq

    private def order: Seq[(String, Int)] => Seq[(String, Int)] = _.sortBy {
      case (_, count) => count
    }.reverse

    private def winner: Seq[(String, Int)] => Option[String] =
      _.headOption.map { case (lang, _) => lang }

    def votingWinner: String =
      (groupByLang andThen sumByLang andThen order andThen winner)(votes)
        .getOrElse("lisp")
  }

  object Option4 {
    def votingWinner: String = "lisp"
  }
}

@main def main(): Unit = {
  import Chains.Option1.*

  println(votingWinner)
}
