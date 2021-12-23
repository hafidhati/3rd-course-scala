
import scala.util.{Try, Failure, Success}
object Adts {

  // a) Дан List[Int], верните элемент с индексом n
  //если лист не пустой то возвращается элемент с нужным индексом иначе none
  def task1(list: List[Int],n: Int):Option[Int]={
     list match {
       case head :: tail => Some(list(n))
       case Nil   => None
       case null  => None
     }
  }
  // примените функцию из пункта (a) здесь, не изменяйте сигнатуру 
  def testGetNth(list: List[Int], n: Int): Option[Int] = task1(list,n)



  // b) Напишите функцию, увеличивающую число в два раза.
  //если передается на пустота то умножаем на 2
  def task2(n: Option[Int]):Option[Int]={
    n match {
      case Some(a) => Some(2*a)
      case None => None
    }
  }
  // примените функцию из пункта (b) здесь, не изменяйте сигнатуру
  def testDouble(n: Option[Int]): Option[Int] = task2(n)



  // c) Напишите функцию, проверяющую является ли число типа Int четным. Если так, верните Right. В противном случае, верните Left("Нечетное число.").
  //используется структура Either,у нее есть функция cond, если в ней выполняется условие то возвращается число иначе строка
  def task3(n: Int):Either[String, Int]= {
    Either.cond(n % 2 == 0, n, "Нечетное число") match {
      case Left(i) => Left(i)
      case Right(s) => Right(s)
    }
  }

    // примените функцию из пункта (c) здесь, не изменяйте сигнатуру
    def testIsEven(n: Int): Either[String, Int] = task3(n)


    // d) Напишите функцию, реализующую безопасное деление целых чисел. Верните Right с результатом или Left("Вы не можете делить на ноль.").
    //используется структура Either,у нее есть функция cond, если в ней выполняется условие то возвращается число иначе строка
    def task4(a: Int, b: Int): Either[String, Int] = {
      Either.cond(b != 0, a / b, "Нельзя делить на ноль") match {
        case Left(i) => Left(i)
        case Right(s) => Right(s)
      }
    }

      // примените функцию из пункта (d) здесь, не изменяйте сигнатуру
      def testSafeDivide(a: Int, b: Int): Either[String, Int] = task4(a, b)


      // e) Обработайте исключения функции с побочным эффектом вернув 0.
      //используется try, в него передает функцию, если она выполняется то возвращается результат
      // если не выполняется то возвращается ошибка
      def task5(impure: String => Int, str: String): Try[Int] = {
        Try(impure(str)) match {
          case Success(a) => Success(a)
          case Failure(error) => Failure(error)
        }
      }
      // примените функцию из пункта (e) здесь, не изменяйте сигнатуру
      def testGoodOldJava(impure: String => Int, str: String): Try[Int] = task5(impure, str)

      def main(args: Array[String]) = {
        println(testGetNth(List(1,2,3,4,5),4))
        println(testDouble(Some(345)))
        println(testIsEven(535))
        println(testSafeDivide(12,5))
        println(testSafeDivide(12,0))
        def func1(str:String):Int=str(0)/0
        println(testGoodOldJava(func1,"String"))
      }
}