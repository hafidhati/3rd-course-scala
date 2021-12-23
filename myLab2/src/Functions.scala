object
Functions {

  /* a) Напишите функцию, которая рассчитывает площадь окружности r^2 * Math.PI
  *
  * ффункция принимает значение типа Double и вычисляет площадь окруности по формуле, которая дана в условии
  * */
  def task1(r:Double):Double={Math.pow(r,2) * Math.PI}
  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
  def testCircle(r: Double): Double = task1(r)
  

  /* b) Напишите карированную функцию которая рассчитывает площадь прямоугольника a * b.
  * Кариррованная функция расчитывает площать прямоугольника
  * Так как функция кариррованная, в нее можно передать сначала один агрумент, а второй передать потом
  * */
  def task2 (a:Double)(b:Double): Double = a * b
  // примените вашу функцию из пукта (b) здесь, не изменяя сигнатуру
  def testRectangleCurried(a: Double, b: Double): Double = task2(a)(b)


  // c) Напишите не карированную функцию для расчета площади прямоугольника.
  // обычная функция перемножает две стороны прямоугольника и получает площадь
  def task3 (a:Double, b:Double): Double = a * b
  // примените вашу функцию из пункта (c) здесь, не изменяя сигнатуру
  def testRectangleUc(a: Double, b: Double): Double = task3(a,b)

  def main(args: Array[String]): Unit ={

    Console.println(testCircle(4))
    Console.println(testRectangleUc(5,2))
    Console.println(testRectangleCurried(5,2))

  }
}

