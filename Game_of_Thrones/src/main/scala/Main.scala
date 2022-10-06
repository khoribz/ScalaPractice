trait Wealth {
  var number_of_soldiers: Int
  var money: Int
}

trait GreatHouse {
  val name: String
  var wealth: Wealth
}

////////////////////////////////////////////////////////////////////////////////////////////
trait MakeWildFire {
  self: GreatHouse =>
  def makeWildFire(number_of_new_soldiers: Int): Wealth = {
    new Wealth {
      override var number_of_soldiers: Int = this.money
      override var money: Int = this.number_of_soldiers + number_of_new_soldiers
    }
  }
}

trait BorrowMoney {
  self: GreatHouse =>
  def borrowMoney(new_money: Int): Wealth = {
    new Wealth {
      override var number_of_soldiers: Int = _
      override var money: Int = this.money + new_money;
    }
  }
}

trait CallDragon {
  self: GreatHouse =>
  def callDragon(): Wealth = {
    new Wealth {
      override var number_of_soldiers: Int = 2 * this.number_of_soldiers
      override var money: Int = _
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////
case class Targaryen(name: String, wealth: Wealth) extends GreatHouse with BorrowMoney with CallDragon {
  def wealth_=(x$1: Wealth): Unit = ???
  def strategy(new_money: Int): Wealth = {
    val new_money_value = new_money / 4
    borrowMoney(new_money_value)
    callDragon()
    wealth
  }
}

case class Lannisters(name: String, wealth: Wealth) extends GreatHouse with BorrowMoney with MakeWildFire {
  def wealth_=(x$1: Wealth): Unit = ???
  def strategy(new_money: Int, number_of_soldiers: Int): Wealth = {
    val new_money_value = new_money / 3
    borrowMoney(new_money_value)
    makeWildFire(number_of_soldiers)
    wealth
  }
}
////////////////////////////////////////////////////////////////////////////////////////////

class GameOfThrones(first_team: Lannisters, second_team: Targaryen, step: Int) {
  def nextTurn(strategy1: => Wealth, strategy2: => Wealth): GameOfThrones = {
    if (step % 2 == 1) new GameOfThrones(Lannisters.apply(first_team.name, strategy1), second_team, step + 1)
    else new GameOfThrones(first_team, Targaryen.apply(second_team.name, strategy2), step + 1)
  }
}
////////////////////////////////////////////////////////////////////////////////////////////

object main extends App {
  var lannisters_team = new Lannisters("Lannisters", new Wealth {
    override var number_of_soldiers: Int = 14
    override var money: Int = 200
  })
  var targaryen_team = new Targaryen("Targaryen", new Wealth {
    override var number_of_soldiers: Int = 15
    override var money: Int = 180
  })
  var lannisters_new_money = 40;
  var lannisters_new_soldiers = 5;
  var targaryen_new_money = 50
  var game = new GameOfThrones(lannisters_team, targaryen_team, 0)
  game = game.nextTurn(lannisters_team.strategy(lannisters_new_money, lannisters_new_soldiers),
    targaryen_team.strategy(targaryen_new_money))
  lannisters_new_soldiers += 4
  lannisters_new_money += 50
  targaryen_new_money += 100
  game = game.nextTurn(lannisters_team.strategy(lannisters_new_money, lannisters_new_soldiers),
    targaryen_team.strategy(targaryen_new_money))
}
