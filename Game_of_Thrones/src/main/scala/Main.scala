trait Wealth {
  def numberOfSoldiers: Int

  def money: Int
}

trait GreatHouse {
  val name: String

  def wealth: Wealth
}

final case class GreatWealth(override val numberOfSoldiers: Int, override val money: Int)
  extends Wealth

////////////////////////////////////////////////////////////////////////////////////////////
trait MakeWildFire {
  self: GreatHouse =>
  def makeWildFire(numberOfSoldiers: Int): Wealth =
    GreatWealth(wealth.money, wealth.numberOfSoldiers + numberOfSoldiers)
}

trait BorrowMoney {
  self: GreatHouse =>
  def borrowMoney(newMoney: Int): Wealth =
    GreatWealth(wealth.money + newMoney, wealth.numberOfSoldiers)
}

trait CallDragon {
  self: GreatHouse =>
  def callDragon(): Wealth =
    GreatWealth(wealth.money, wealth.numberOfSoldiers * 2)
}

////////////////////////////////////////////////////////////////////////////////////////////
final case class Targaryen(wealth: Wealth)
  extends GreatHouse
    with BorrowMoney
    with CallDragon {
  override val name = "Targaryen"

  def strategy(new_money: Int): Wealth = {
    val new_money_value = new_money / 4
    borrowMoney(new_money_value)
    callDragon()
    wealth
  }
}

final case class Lannisters(wealth: Wealth)
  extends GreatHouse
    with BorrowMoney
    with MakeWildFire {
  override val name = "Lannisters"

  def strategy(new_money: Int, numberOfSoldiers: Int): Wealth = {
    val new_money_value = new_money / 3
    borrowMoney(new_money_value)
    makeWildFire(numberOfSoldiers)
    wealth
  }
}
////////////////////////////////////////////////////////////////////////////////////////////

case class GameOfThrones(lannisters: Lannisters, targaryen: Targaryen, step: Int = 0) {
  def nextTurn(lannistersMove: Lannisters => Wealth)(targaryenMove: Targaryen => Wealth): GameOfThrones = {
    GameOfThrones(lannisters.copy(wealth = lannisters.strategy(200, 20)),
      targaryen.copy(wealth = targaryen.strategy(400)), step + 1)
  }
}
////////////////////////////////////////////////////////////////////////////////////////////

object main extends App {
  GameOfThrones(
    Lannisters(GreatWealth(20, 300)),
    Targaryen(GreatWealth(15, 500)))
    .nextTurn(lan => lan.borrowMoney(30))(tar => tar.borrowMoney(40))
}
