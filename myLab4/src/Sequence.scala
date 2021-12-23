
import scala.annotation.tailrec
object Sequence {

  /* a) Найдите последний элемент Seq.
  *
  * рекурсивно перебираются все элементы, когда найден последний, его возвращаем
  * */
  def testLastElement[A](seq:Seq[A]):Option[A]={
    @tailrec
    def loop(myseq:Seq[A]):Option[A]={
      myseq match{
        case Nil => None
        case Seq(a)=>Some(a)
        case _::tail=>loop(tail)
      }
    }
    loop(seq)
  }

  /* b) Объедините две Seqs (то есть Seq(1, 2) и Seq(3, 4) образуют Seq((1, 3), (2, 4))) - если Seq длиннее игнорируйте оставшиеся элементы.
  *
  * рекурсивно в новый Seq добавляются пары (одно значение из первого списка, одно из второго)
  * если в каком то из списков заканчиваются значение, а в другом нет, то значени обрезаются
  * */

  def testZip2[A](a: Seq[A], b: Seq[A]): Seq[(A, A)] = {
    @tailrec
    def loop(a1: Seq[A], b1: Seq[A],res:Seq[(A, A)]):Seq[(A, A)]={
      a1 match {
        case head1::Nil => b1 match {
          case Nil=>res
          case head2::_=>res:+(head1,head2)
        }
        case head1::tail1=>b1 match {
          case Nil=>res
          case head2::tail2=>loop(tail1,tail2,res:+(head1,head2))
        }
        case Nil =>res
      }
    }
    loop(a,b,Nil)
  }

  /* c) Проверьте, выполняется ли условие для всех элементов в Seq.
  *
  * рекурсивно берется каждый элемент, в функцию передается это значение
  * к резульатту функции применяется оператор && с true
  * если в условии тоже true то истина сохраняется, если будет хоть один false то все будет ложью
  * */
  def testForAll[A](seq: Seq[A])(cond: A => Boolean): Boolean = {
    @tailrec
    def loop(myseq:Seq[A],res:Boolean):Boolean={
      myseq match {
        case head::tail=>loop(tail,res && cond(head))
        case Nil=>res
      }
    }
    loop(seq,true)
  }

  /* d) Проверьте, является ли Seq палиндромом
  *
  * рекурсивно проверяется равно ли последнее и первое число, если да, то счетчик увеличивается
  * после этого список обрезается на 1 элемент и слева и справа
  * в конце если счетчик равен половине длины Seq то строка является палиндромом
  * */
  def testPalindrom[A](seq: Seq[A]): Boolean = {
    @tailrec
    def loop(myseq:Seq[A],numb:Int):Int={
      myseq.length match {
        case 0 => numb
        case 1 => numb
        case _ => if (myseq.head==myseq.last) loop(myseq.tail.init,numb+1) else numb
      }
    }
    return loop(seq,0)==seq.length/2
  }

  /* e) Реализуйте flatMap используя foldLeft.*/
  def testFlatMap[A, B](seq: Seq[A])(f: A => Seq[B]): Seq[B] = seq.foldLeft(Seq[B]())((x,y)=>x++:f(y))


  def main(args: Array[String]) = {

    var seq1=Seq(1,2,3,4,5,4,3,2,1)
    var seq2=Seq(1,2,3,4,5,6)
    def func1(num:Int):Boolean=num==5
    def func2(x:Int):Seq[Double]=Seq(x,x*x+1)

    println(testLastElement(seq1))
    println(testZip2(seq1,seq2))
    println(testForAll(seq1)(func1))
    println(testPalindrom(seq1))
    println(testPalindrom(seq2))
    println(testFlatMap(seq1)(func2))
  }
}
