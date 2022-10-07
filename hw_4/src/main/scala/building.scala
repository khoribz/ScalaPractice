sealed trait Gender
case object Man extends Gender
case object Woman extends Gender
case class Inhabitance(age: Int, gender: Gender)

sealed trait Floor
case class LivingFloor(person1: Inhabitance, person2: Inhabitance, floor: Floor) extends Floor
case object Attic extends Floor

case class Building(address: String, floor: Floor)

class BuildingFunctions {
  def protoFold(building: Building, acc0: Int)(f: (Int, LivingFloor) => Int): Int = {
    building.floor match {
      case Attic => acc0
      case livingFloor: LivingFloor =>
        protoFold(building.copy(floor = livingFloor.floor), f(acc0, livingFloor))(f)
    }
  }
  def countOldManFloors(building: Building, olderThen: Int): Int = {
    protoFold(building, 0) {
      case (currentOldManNumber, LivingFloor(Inhabitance(age, Man), _, _))
        if age > olderThen => currentOldManNumber + 1
      case (currentOldManNumber, LivingFloor(_, Inhabitance(age, Man), _))
        if age > olderThen => currentOldManNumber + 1
      case (currentOldManNumber, _) => currentOldManNumber
    }
  }
  def womanMaxAge(building: Building):Int = {
    protoFold(building, 0){
      case (currentWomanMaxAge, LivingFloor(Inhabitance(age1, Woman), Inhabitance(age2, Woman), _)) =>
        currentWomanMaxAge max age1 max age2
      case (currentWomanMaxAge, LivingFloor(Inhabitance(age, Woman), _, _)) =>
        currentWomanMaxAge max age
      case (currentWomanMaxAge, LivingFloor(_, Inhabitance(age, Woman), _)) =>
        currentWomanMaxAge max age
      case (currentWomanMaxAge, _) => currentWomanMaxAge
    }
  }
}

