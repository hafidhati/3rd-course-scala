
object PatternMatching {

  sealed trait Hand
  case object Rock    extends Hand
  case object Paper   extends Hand
  case object Scissor extends Hand

  sealed trait Result
  case object Win  extends Result
  case object Lose extends Result
  case object Draw extends Result

  sealed trait Food
  case object Meat       extends Food
  case object Vegetables extends Food
  case object Plants     extends Food

   trait Animal {

    val name: String
    var food: Food
  }

  case class Mammal(name: String, var food: Food, weight: Int) extends Animal
  case class Fish(name: String, var food: Food)                extends Animal
  case class Bird(name: String, var food: Food)                extends Animal


  /* a) Напишите функцию, которая ставит в соответствие числу строку слудеющим образом:
   * Если:
   *     1 => "it is one"
   *     2 => "it is two"
   *     3 => "it is three"
   *     иначе => "what's that"
   */
  def intToString(n: Int): String = n match {
    case 1 => "it is one"
    case 2 => "it is two"
    case 3 => "it is three"
    case _ => "what's that"
  }
  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
  def testIntToString(value: Int): String = intToString(value)



  /* b) Напишите функцию которая возвращает true если переменная `value` принимает значение:
   *     "max" или "Max
   *     "moritz" или "Moritz"
   */
  def isMaxAndMoritz(s: String): Boolean = s match {
    case "moritz" | "Moritz" | "max" | "Max"  => true
    case _ => false
  }
  // примените функции из пункта (b) здесь, не изменяя сигнатуру
  def testIsMaxAndMoritz(value: String): Boolean = isMaxAndMoritz(value)


  // c) Напишите функцию проверки является ли `value` четным 
  def isEven(n: Int): Boolean = n % 2 == 0
  // примените функции из пункта (c) здесь, не изменяя сигнатуру
  def testIsEven(value: Int): Boolean = isEven(value)



  /* d) Напишите функцию, моделирующую игру в Камень ножницы бумага 
   *     1. камень побеждает ножницы
   *     2. ножницы побеждают бумагу
   *     3. бумага побеждает камень
   *    Выиграет ли игрок `a`?
   *
   *
   * перебираются все возможные комбинации и определяется выигрыш в соответствии с правилами игры
   */
  def winsA(a: Hand, b: Hand): Result = (a, b) match {
    case (Scissor, Paper) | (Paper, Rock) | (Rock, Scissor) => Win
    case (Rock, Rock) | (Scissor, Scissor) | (Paper, Paper) => Draw
    case _ => Lose
  }
  // примените вашу функцию из пункта (d) здесь, не изменяя сигнатуру
  def testWinsA(a: Hand, b: Hand): Result = winsA(a,b)



  // e) Верните вес (weight: Int) объекта Mammal, иначе верните -1.
  def extractMammalWeight(animal: Animal): Int = animal match {
    case animal : Mammal => animal.weight
    case _ => -1
  }
  // примените функцию из пункта (e) здесь, не изменяйте сигнатуру
  def testExtractMammalWeight(animal: Animal): Int = extractMammalWeight(animal)


  // f) Измените поле еда объектов классов Fishes и Birds на Plants, класс Mammels оставьте неизмененным.
  def updateFood(animal: Animal): Animal = animal match{
    case animal : Mammal => animal
    case _ =>
      animal.food = Plants
      animal
  }
  // примените функцию из пункта (f) здесь, не изменяйте сигнатуру
  def testUpdateFood(animal: Animal): Animal = updateFood(animal)

  def main(args: Array[String]): Unit = {
    val mamal = new Mammal("Person", Meat,56)
    println(testIntToString(2))
    println(testIsMaxAndMoritz("Moritz"))
    println(testIsEven(2))
    println(testWinsA(Rock, Paper))
    println(testExtractMammalWeight(mamal))
    println(testUpdateFood(mamal).food)

  }
}
