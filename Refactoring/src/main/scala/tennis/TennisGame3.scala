package tennis
import scala.util.matching.Regex
trait Stage
case object CoreStage extends Stage
case object AdvancedStage extends Stage
case object Deuce extends Stage
case class TennisGame3(playerOneName: String, playerTwoName: String)
  extends TennisGame {
  private val playerOne = new Player(playerOneName)
  private val playerTwo = new Player(playerTwoName)
  def calculateScore(): String = determineStage match {
    case Deuce => "Deuce"
    case CoreStage => if (equalScore) playerOne.score + "-All"
                      else playerOne.score + "-" + playerTwo.score
    case AdvancedStage => if (differenceScore == 1) "Advantage " + leader
                          else "Win for " + leader
  }
  def wonPoint(playerName: String): Unit = {
    if (playerName == playerOneName)
      playerOne.wonPoint()
    else
      playerTwo.wonPoint()
  }

  private def determineStage: Stage = {
    if (isDeuce) Deuce
    else if (coreStage) CoreStage
    else AdvancedStage
  }
  private def coreStage =
    playerOne.points < 4 && playerTwo.points < 4

  private def isDeuce =
    equalScore && playerTwo.points >= 3

  private def equalScore =
    playerOne.points == playerTwo.points

  private def differenceScore =
    Math.abs(playerOne.points - playerTwo.points)

  private def leader =
    if (playerOne.points > playerTwo.points) playerOneName
    else playerTwoName
}

private class Player(val name: String) {
  private var _points = 0

  def score: String = _points match {
    case 0 => "Love"
    case 1 => "Fifteen"
    case 2 => "Thirty"
    case 3 => "Forty"
    case _ => "Advantage|Win".r
  }

  def points: Int = _points

  def wonPoint(): Unit = {
    _points += 1
  }
}
