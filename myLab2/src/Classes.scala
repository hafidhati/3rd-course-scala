/*
 a) Создать класс Animal, который имеет следующие поля:
 *      - name: String (название)
 *      - species: String (вид)
 *      - food: String
 * 
 *    Синтаксис: class MyClass(val publicField: Int, privateField: String) {
 *              // остальные поля и методы
 *            }
 *   d) Переопределите ваш класс Animal как трейт и создайте объекты класса-образца для Mammals, Birds и Fishs.
 *    Вам все еще нужно поле `species`?
 */

//создан трейт с полями name, food и с функцией eats
trait Animal {
  val name: String
  val food: Food

  /*c) Добавьте def eats(food: String): Boolean который проверяет ест ли животное определенную пищу */
  //поле val food: Food сравнивается с параметром функции food: Food
  def eats(food: Food): Boolean = food == this.food
}

// созданы кр=лассы, которые наследуются от трейта Animal
case class Mammal(name: String, food: Food, weight: Int) extends Animal
case class Fish(name: String, food: Food)                extends Animal
case class Bird(name: String, food: Food)                extends Animal

/*b) Создайте объект-компаньон для класса Animal и добавьте следующие сущности как поля:
*      - cat, mammal, meat
*      - parrot, bird, vegetables
*      - goldfish, fish, plants
*
*    Синтаксис: object MyClass {
*              // статические поля и методы
*            }
*/

//создан объект-компаньон Animal
 object Animal{
   /* e) Добавьте следующие функции в объект-компаньон Animal:
*      def knownAnimal(name: String): Boolean  // true если это имя одного из трех животных из (b)
*      def apply(name: String): Option[Animal] // возвращает одно из трех животных в соответствии с именем (Some) или ничего (None), см. ниже
*/

  //если имена животного ровны  cat,parrot или goldfish то возвращается истина
   def knownAnimal(name: String): Boolean = name == "cat" | name ==  "parrot"| name == "goldfish"

   //функция apply принимает строку и возвращает созданный экземпляр обьекта
   def apply(name: String): Option[Animal] = name match{
     case "cat" => Some(Mammal("cat", Meat, 3))
     case "parrot" => Some(Bird("parrot", Vegetables))
     case "goldfish" => Some(Fish("goldfish", Plants))
     case _ => None
   }
 }


 /* f) Создайте трейт Food со следующими классами-образцами:
 *      - Meat
 *      - Vegetables
 *      - Plants
 *   и добавьте это в определение Animal. Так же добавьте объект-компаньон с методом apply():
 *      def apply(food: String): Option[Food]
 */

//создан трейт Food
 trait Food

//созданы классы, которые наследуются от трейта Food
case object Meat       extends Food
case object Vegetables extends Food
case object Plants     extends Food

//создан обьект компаньон
object Food{
  //функция apply принимает строку и возвращает созданный экземпляр обьекта
  def apply(food: String): Option[Food] = food match{
    case "Meat" => Some(Meat)
    case "Vegetables" => Some(Vegetables)
    case "Plants" => Some(Plants)
    case _ => None
  }

  def main(args: Array[String]): Unit = {

    println(Animal.knownAnimal("goldfish"))
    println(Animal.apply("goldfish"))
    println(Food.apply("Meat"))
  }
}

