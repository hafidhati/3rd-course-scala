package myLab3

//создан трейт List
sealed trait List[A]
//созданы два класса, которые наследуются от этого трейта
//класс Cons преставляет непустой лист, у которого хранится первый элемент отдельно от всех остальных
case class Cons[A](head: A, tail: List[A]) extends List[A]
//класс Nil представляет пустой лист
case class Nil[A]() extends List[A]

/** Напишите свои решения в виде функций. */
object RecursiveData {

  // a) Реализуйте функцию, определяющую является ли пустым `List[Int]`.
  //просто сравнение с пустым листом
  def listIntEmpty(list: List[Int]): Boolean = list == Nil()
  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testListIntEmpty(list: List[Int]): Boolean = listIntEmpty(list)

  // b) Реализуйте функцию, которая получает head `List[Int]`или возвращает -1 в случае если он пустой.
  //сравнение с непустым листом Cons
  //если данный лист не пустой, то возвращаем первый элемент
  def listIntHead(list: List[Int]): Int = list match {
    case list: Cons[Int] => list.head
    case _ => -1
  }
  // используйте функцию из пункта (б) здесь, не изменяйте сигнатуру
  def testListIntHead(list: List[Int]): Int = listIntHead(list)

  // c) Можно ли изменить `List[A]` так чтобы гарантировать что он не является пустым?
  //вместа класса Nil, который представляет пустой лист создаем класс End, в котором есть значение head
  sealed trait ListWithEnd[A]
  case class ConsWithEnd[A](head: A, tail: ListWithEnd[A]) extends ListWithEnd[A]
  case class End[A](head: A) extends ListWithEnd[A]

  /* d) Реализуйте универсальное дерево (Tree) которое хранит значения в виде листьев и состоит из:
   *      node - левое и правое дерево (Tree)
   *      leaf - переменная типа A
   */
  sealed trait Tree[A]
  case class Node[A](leftBranch: Tree[A], rightBranch: Tree[A]) extends Tree[A]
  case class Leaf[A](head: A) extends Tree[A]




  def main(args: Array[String]): Unit = {

    var list1: List[Int] = Cons(45, Cons(5, Nil()))
    var list2: List[Int] = Nil()

    println(testListIntEmpty(list1))
    println(testListIntEmpty(list2))

    println(testListIntHead(list1))
    println(testListIntHead(list2))


    println(
      Node(
           Node(Leaf(3),
                Leaf(2)),
           Node(Leaf(9),
                Node(Leaf(8),
                     Leaf(7)))))
  }
}
