/** Напишите ваши решения в виде функций. */
object HigherOrder {

  val plus: (Int, Int) => Int = _ + _
  val multiply: (Int, Int) => Int = _ * _

  /* a) Напишите функцию, которая принимает `f: (Int, Int) => Int`, параменты `a` и `b`
   *    и коэффициент умножения `n` и возвращает n * f(a, b). Назовите `nTimes`.
   *
   * данная функци принимает в качестве аргументов другую функцию
   * и выполняет умножение числа n на результат функции f, в которую передаем a,b
   * */
  def nTimes(f: (Int, Int) => Int, a: Int, b: Int, n: Int): Int = n * f(a, b)
  // примените вашу функцию (a) здесь, не изменяйте сигнатуру
  def testNTimes(f: (Int, Int) => Int, a: Int, b: Int, n: Int): Int = nTimes(f,a,b,n)


  /* b) Напишите анонимную функцию, функцию без идентификатора ((a, b) => ???) для `nTimes` которая
   *    выполняет следующее:
   *          if (a > b) a else b
   *
   * в этой функции выполняется функция nTimes,
   * в качестве функции f  в нее передается анонимная функция if (a > b) a else b и остальные параметры я a,b,n
   */
  def testAnonymousNTimes(a: Int, b: Int, n: Int): Int = testNTimes((a: Int, b: Int) => if (a > b) a else b, a, b, n)

  def main(args: Array[String]): Unit = {
    println(testNTimes(plus, 1,2,3))
    println(testNTimes(multiply,4,5,6))
    println(testAnonymousNTimes(7,8,9))
  }
}
