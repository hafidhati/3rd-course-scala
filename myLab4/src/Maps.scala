object Maps {

  case class User(name: String, age: Int)

  /* a) В данной Seq[User] сгруппируйте пользователей по имени (`groupBy`) и вычислите средний возраст: `name -> averageAge`*/
  //группируется по полю имя  с помощью groupBy
  //применяется функция для вычисления среднего значения
  //каждому имени ставится соответствующее среднее значение возраста
  def testGroupUsers(users: Seq[User]): Map[String, Int] = {
    def srage(num:Seq[Int])=num.sum/num.length

    users.groupBy(_.name).map{
      myKey=>(myKey._1, srage(myKey._2.map(_.age)))
    }
  }


  /* b) Дана `Map[String, User]` состоящая из имен пользователей `User`, сколько имен пользователей, содержащихся в Map, содержат подстроку "Adam"?*/
  //используется функция count  с условием, что имя=Adam
  def testNumberFrodos(map: Map[String, User]): Int = {
    map.count(x=>x._2.name=="Adam")
  }


  /* c) Удалите всех пользователей возраст которых менее 35 лет.*/
  //используется функция filter с учловием, что выбирааются значения, у которых возраст больше 35
  def testUnderaged(map: Map[String, User]): Map[String, User] = {
      map.filter(x=>x._2.age>35)
  }

  def main(args: Array[String]) = {
    println(testGroupUsers(Seq(User("Hafi",13),User("Hafi",22))))
    var map=Map("Peson1"->User("Adam",5),"Person2"->User("Adam",74))
    println(testNumberFrodos(map))
    println(testUnderaged(map))
  }
}
