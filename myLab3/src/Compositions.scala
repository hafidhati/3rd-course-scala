package myLab3
sealed trait Option[A] {

  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
}
case class Some[A](a: A) extends Option[A] {

  def map[B](f: A => B): Option[B] = Some(f(a))
  def flatMap[B](f: A => Option[B]): Option[B] = f(a)
}
case class None[A]()     extends Option[A] {

  def map[B](f: A => B): Option[B] = None()
  def flatMap[B](f: A => Option[B]): Option[B] = None()
}

/** Напишите ваши решения в тестовых функциях.  */
object Compositions {

  // a) Используйте данные функции. Вы можете реализовать свое решение прямо в тестовой функции.
  // Нельзя менять сигнатуры 

  def testCompose[A, B, C, D](f: A => B)
                             (g: B => C)
                             (h: C => D): A => D = h.compose(g).compose(f)

    /**
     * (f: A => B)
     * (g: B => C)
       (h: C => D) это значит, что функция g примет параметр типа В, который получился в результате функции f
                   а функция h примет параметр C который получился в результате функции g

   * Происходит объединение функций с помощью функции compose
   */


  // b) Напишите функции с использованием `map` и `flatMap`. Вы можете реализовать свое решение прямо в тестовой функции.
  // Нельзя менять сигнатуры 

  def testMapFlatMap[A, B, C, D](f: A => Option[B])
                                (g: B => Option[C])
                                (h: C => D): Option[A] => Option[D] = _ flatMap f flatMap g map h

  // функции flatMap и map берут значение слева и применяют к нему функцию справа
  // так постепенно выполняются три функции : f,g,h и получается конечный результат


  // c) Напишите функцию используя for. Вы можете реализовать свое решение прямо в тестовой функции.
  // Нельзя менять сигнатуры 

  def testForComprehension[A, B, C, D](f: A => Option[B])
                                      (g: B => Option[C])
                                      (h: C => D): Option[A] => Option[D] = {
    for {
      a <- _//в а помещается значение типа A
      b <- f(a)//в b помещается значение результата функции f от значения a
      c <- g(b)//в с помещается значение результата функции g от значения b
    } yield h(c)//yield  возвращает результат функции h от значения c
    //так постепенно выполняются три функции : f,g,h и получается конечный результат


  }

  def Func1(x: Char): Int = x+x
  def Func2(x: Int): Double = x+x+x
  def Func3(x: Double): Int = (x / 3).toInt
  def Func4(x: Char): Option[Int] = Some(x+x)
  def Func5(x: Int): Option[Double] = Some(x+x+x)
  def Func6(x: Double): Int = (x / 3).toInt

  def main(args: Array[String]): Unit = {
    println(testCompose(Func1)(Func2)(Func3)('g'))
    println(testMapFlatMap(Func4)(Func5)(Func6)(Some('g')))
    println(testForComprehension(Func4)(Func5)(Func6)(Some('g')))
  }
}
