package myLab3

import scala.annotation.tailrec

object RecursiveFunctions {

  def length[A](as: List[A]): Int = {
    @tailrec
    def loop(rem: List[A], agg: Int): Int = rem match {
      case Cons(_, tail) => loop(tail, agg + 1)
      case Nil()         => agg
    }

    loop(as, 0)
  }

  /* a) Напишите функцию которая записывает в обратном порядке список:
   *        def reverse[A](list: List[A]): List[A]
   *
   * рекурсивно берется первый элемент листа и ставится в конец результирующего листа
   * после этого лист уменьшается
   */
  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def loop(origin: List[A], reverse: List[A]): List[A] = origin match {
      case Cons(n, t) => loop(t, Cons(n, reverse))
      case Nil() => reverse
    }
    loop(list, new Nil[A])
  }
  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testReverse[A](list: List[A]): List[A] = reverse(list)


  /* b) Напишите функцию, которая применяет функцию к каждому значению списка:
   *        def map[A, B](list: List[A])(f: A => B): List[B]
   *
   * рекурсивно выделяется каждый элемент списка и к нему применяется функция, резултат записывается в новый список
   */
  def map[A, B](list: List[A])(f: A => B): List[B] = {
    @tailrec
    def loop(origin: List[A], changed: List[B]): List[B] = origin match {
      case Cons(n, t) => loop(t, Cons(f(n), changed))
      case Nil() => reverse(changed)
    }
    loop(list, Nil())
  }
  // используйте функцию из пункта  (b) здесь, не изменяйте сигнатуру
  def testMap[A, B](list: List[A], f: A => B): List[B] = map(list)(f)


  /* c) Напишите функцию, которая присоединяет один список к другому:
   *        def append[A](l: List[A], r: List[A]): List[A]
   *
   * рекурсивно берем каждый элемент левого листа и добавляем в начало правого листа
   * предварительно к левому листу применяется реверс
   */
  def append[A](l: List[A], r: List[A]): List[A] = {
    @tailrec
    def loop(left: List[A], right: List[A] ): List[A] = left match {
      case Cons(n, t) => loop(t, Cons(n, right))
      case Nil() => right
    }
    loop(reverse(l), r)
  }

  // используйте функцию из пункта  (c) здесь, не изменяйте сигнатуру
  def testAppend[A](l: List[A], r: List[A]): List[A] = append(l,r)

  /* d) Напишите функцию, которая применяет функцию к каждому значению списка:
   *        def flatMap[A, B](list: List[A])(f: A => List[B]): List[B]
   * 
   *    она получает функцию, которая создает новый List[B] для каждого элемента типа A в 
   *    списке. Поэтому вы создаете List[List[B]].
   *
   * рекурсивно выделяется каждый элемент списка и к нему применяется функция, резултат записывается в новый список
   * но тут в результате уже получается лист листов
   */
  def flatMap[A, B](list: List[A])(f: A => B): List[B] = {
    @tailrec
    def loop(origin: List[A], result: List[B]): List[B] = origin match {
      case Cons(n, t) => loop(t, Cons(f(n), result))
      case Nil() => reverse(result)
    }
    loop(list,new Nil[B])
  }
  // используйте функцию из пункта (d) здесь, не изменяйте сигнатуру
  def testFlatMap[A, B](list: List[A], f: A => List[B]): List[B] = Nil()

  /* e) Вопрос: Возможно ли написать функцию с хвостовой рекурсией для `Tree`s? Если нет, почему? */
  /** Если необходимо найти какое-то значение, тогда можно написать функцию с хвостовой рекурсие.
   * Если необходимо проверить оба значения в Node, тогда реализация может стать невыполнимой
   * потому что понадобится запустить 2 функции (для каждого Node)
   * значений
   */

  def main(args: Array[String]): Unit = {
    println(testReverse(Cons(2, Cons(2, Cons(4, Nil())))))
    println(testMap(Cons(5, Cons(12, Cons(35, Nil()))), (x: Int) => x + 87))
    println(testAppend(Cons[Int](5, Cons(4, Cons(7, Nil()))), Cons[Int](4, Cons(5, Cons(6, Nil())))))
    println(flatMap(Cons[Int](9, Cons(5, Cons(7, Nil()))))((x: Int) => Cons(x, Nil())))
  }
}
