package Lab_ex4

object Typeclasses {

  // a) Определите тайп-класс Reversable, который представляет в обратном порядке значения.

  //создан трейт в котором обьявлена функция
  trait Reversable[T]{def reverse(value:T):T}

  // b) Реализуйте функцию Reverse для String.
  //создан обьект в котором функция реализуется сначала неявно, а затем явно для нужных типов, в данном случае для строки
  object Reversable {
    def reverse[T: Reversable](a: T): T = implicitly[Reversable[T]].reverse(a)
    implicit object ReversableString extends Reversable[String]{
      def reverse(str:String):String=str.reverse
    }
  }
  // примените тайп-класс-решение из пункта (a) здесь
  def testReversableString(mystr: String): String = Reversable.reverse(mystr)

  // c) Определите тайп-класс Smash таким образом чтобы в нем была функция smash, которая выполняет операцию со значениями одного типа.

  //создан трейт в котором обьявлена функция
  trait Smash[T]{def smash(a: T, b: T): T}

  //создан обьект в котором функция реализуется сначала неявно, а затем явно для нужных типов, в данном случае для int,double,string
  object Smash {
    def smash[T : Smash](a: T, b: T): T = implicitly[Smash[T]].smash(a,b)
    implicit object SmashInt extends Smash[Int]{
      def smash(value1:Int,value2:Int):Int=value1+value2
    }
    implicit object SmashDouble extends Smash[Double]{
      def smash(value1:Double,value2:Double):Double=value1+value2
    }
    implicit object SmashString extends Smash[String]{
      def smash(value1:String,value2:String):String=value1+value2
    }
  }

  // d) Реализуйте  функции Smash для типа Int и Double.
  //    Используйте сложение для типа Int у умножение для типа Double.

  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashInt(a: Int, b: Int): Int = Smash.smash(a,b)

  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashDouble(a: Double, b: Double): Double = Smash.smash(a,b)


  // e) Реализуйте функцию Smash для типа String. Необходимо выполнить конкатенацию строк, которые будут получены в качестве параметра. 

  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashString(a: String, b: String): String = Smash.smash(a,b)

  def main(args:Array[String])={

    println(testReversableString("Hafidati"))
    println(testSmashInt(66,12))
    println(testSmashDouble(56,24))
    println(testSmashString("String1","String2"))
  }
}

