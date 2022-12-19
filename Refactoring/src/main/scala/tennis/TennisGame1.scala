package tennis

case class Score(points: Int) {
  val FORTY = "Forty"
  val THIRTY = "Thirty"
  val FIFTEEN = "Fifteen"
  val LOVE = "Love"
  val SUCCESS = "Success"

  def pointsToScore = points match {
    case 0 => LOVE
    case 1 => FIFTEEN
    case 2 => THIRTY
    case 3 => FORTY
    case _ => SUCCESS
  }
}
case class TennisGame1(player1Name: String, player2Name: String)
  extends TennisGame {
  private val SCORE_SEPARATOR = "-"

  private var playerOneScore: Int = 0
  private var playerTwoScore: Int = 0

  def wonPoint(playerName: String): Unit = {
    if (playerName == player1Name)
      playerOneScore += 1
    else
      playerTwoScore += 1
  }
  def calculateScore(): String = {
    if (playerOneScore == playerTwoScore) determineEqualScore
    else if (coreStage(playerOneScore, playerTwoScore))
      formScoreWithoutVictory(playerOneScore, playerTwoScore)
    else formVictoryScore(playerOneScore, playerTwoScore)
  }

  private def coreStage(playerOneScore: Int, playerTwoScore:Int) =
    playerOneScore <= 3 && playerTwoScore <= 3
  private def formScoreWithoutVictory(playerOneScore: Int, playerTwoScore: Int) =
    determinePlayerScore(playerOneScore) +
      SCORE_SEPARATOR +
      determinePlayerScore(playerTwoScore)

  private def formVictoryScore(playerOneScore: Int, playerTwoScore: Int) = {
    val scoreDiff = Math.abs(playerOneScore - playerTwoScore)
    val playerInLead = determinePlayerInLead
    if (scoreDiff == 1) "Advantage " + playerInLead
    else "Win for " + playerInLead
  }
  private def determineEqualScore =
    if (playerOneScore >= 3) "Deuce"
    else determinePlayerScore(playerOneScore) + SCORE_SEPARATOR + "All"

  private def determinePlayerScore(playerPoints: Int) = Score(playerPoints).pointsToScore
  private def determinePlayerInLead =
    if (playerOneScore > playerTwoScore) player1Name
    else if (playerTwoScore > playerOneScore) player2Name
    else "tie"
}